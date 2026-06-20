{- cabal:
build-depends: base, random
-}

import Data.List (find)
import qualified Data.Set as Set
import Control.Monad.State
import System.Random (StdGen, randomR)

type PersonId = Int
type Year     = Int

type SimState = (StdGen, PersonId)

type SimMonad a = State SimState a

data Sex = Male | Female
    deriving (Show, Eq, Enum, Bounded)

data AgeGroup = Child | Youth | Adult | Elder
    deriving (Show, Eq, Enum, Bounded)

data DiscoveryType = Technology | Magic | Culture
    deriving (Show, Eq)

data Discovery = Discovery
    { discoveryName :: String
    , discType      :: DiscoveryType
    , mortalityMod  :: Double      -- e.g., -0.05 (reduces mortality by 5%)
    , birthRateMod  :: Double      -- e.g., +0.02 (increases birth rate by 2%)
    , prerequisites :: [String]    -- Names of discoveries needed first
    } deriving (Show, Eq)

data Person = Person
    { personId    :: PersonId
    , name        :: String
    , age         :: Int
    , sex         :: Sex
    , parentIds   :: [PersonId]      -- Ancestry tracking! Empty for the first generation
    , specialness :: Int
    } deriving (Show, Eq)

data Population = Population
    { popName           :: String
    , baseBirthRate     :: Double     -- Base percentage (e.g., 0.15)
    , baseMortalityRate :: Double     -- Base percentage (e.g., 0.10)
    , people            :: [Person]
    , discoveries       :: [Discovery]
    } deriving (Show, Eq)

data World = World
    { currentYear :: Year
    , populations :: [Population]
    } deriving (Show, Eq)


-- Person functions
ageGroup :: Person -> AgeGroup
ageGroup Person { age=a }
    | a < 6 = Child
    | a < 18 = Youth
    | a < 50 = Adult
    | otherwise = Elder

agePerson :: Person -> Person
agePerson p = p { age = age p + 1 }

canReproduce :: Person -> Bool
canReproduce p = eq (ageGroup p) Adult

findPerson :: PersonId -> [Person] -> Maybe Person
findPerson pid = find (\p -> personId p == pid) 

getAncestors :: Int -> PersonId -> [Person] -> Set.Set PersonId
getAncestors depth pid pool
    | depth < 0 = Set.empty
    | otherwise = case findPerson pid pool of
        Nothing -> Set.singleton pid
        Just p ->
            let parentTrees = [ getAncestors (depth - 1) parentId pool | parentId <- parentIds p]
            in Set.insert pid (Set.unions parentTrees)

areUnrelated :: PersonId -> PersonId -> [Person] -> Bool
areUnrelated id1 id2 pool = 
    let tree1 = getAncestors 2 id1 pool -- self, parents, grandparents
        tree2 = getAncestors 2 id2 pool
    in Set.null (Set.intersection tree1 tree2)

-- Population functions
currentMortalityRate :: Population -> Double
currentMortalityRate pop =
    let base = baseMortalityRate pop
        mods = sum [ mortalityMod d | d <- discoveries pop ]
    in  max 0.0 (base + mods) -- ensure mortality never goes below 0%

-- advancePopulation :: Population -> SimMonad Population
-- advancePopulation pop = do
--     -- Age em up
--     let agedPeople = map (\p -> p { age = age p + 1 }) (people pop)

--     -- TODO: filter for survivors


-- Stateful stuff

freshId :: SimMonad PersonId
freshId = do
    (gen, nextId) <- get
    put (gen, nextId + 1)
    return nextId

rollRange :: (Double, Double) -> SimMonad Double
rollRange (low, high) = do
    (gen, nextId) <- get
    let (val, nextGen) = randomR (low, high) gen
    put (nextGen, nextId)
    return val