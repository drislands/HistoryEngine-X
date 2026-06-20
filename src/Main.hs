{- cabal:
build-depends: base, random
-}

import Data.List (find)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad (forM)
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
canReproduce p = ageGroup p == Adult

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

currentBirthRate :: Population -> Double
currentBirthRate pop =
    let base = baseBirthRate pop
        mods = sum [ birthRateMod d | d <- discoveries pop ]
    in  max 0.0 (base + mods)

advancePopulation :: Population -> SimMonad Population
advancePopulation pop = do
    -- Age em up
    let agedPeople = map (\p -> p { age = age p + 1 }) (people pop)

    -- Who dies?
    survivingPeople <- rollForDeath (currentMortalityRate pop) agedPeople

    -- Who is born?
    newBabies <- generateOffspring (currentBirthRate pop) survivingPeople

    -- Update the population with the survivors
    return pop { people = survivingPeople ++ newBabies }

rollForDeath :: Double -> [Person] -> SimMonad [Person]
rollForDeath rate = filterM checkSurvival
    where
        checkSurvival :: Person -> SimMonad Bool
        checkSurvival person = do
            roll <- rollDoubleRange (0.0, 1.0)
            return (roll < rate)

generateOffspring :: Double -> [Person] -> SimMonad [Person]
generateOffspring birthRate pool = do
    let females = [p | p <- pool, sex p == Female]
        males   = [p | p <- pool, sex p == Male]
    maybeBabies <- forM females $ \mom -> do
        -- Filter out the male pool to individuals unrelated to this female
        let validDads = filter (\m -> areUnrelated (personId mom) (personId m) pool) males
        if null validDads
            then return Nothing
            else do
                birthRoll <- rollDoubleRange (0.0, 1.0)
                if birthRoll > birthRate
                    then return Nothing
                    else do
                        dad    <- rollListSelection validDads
                        newId  <- freshId
                        newSex <- rollSex
                        spec   <- calculateSpecialness
                        return $ Just Person
                            { personId = newId
                            , name = "whatever"
                            , age  = 0
                            , sex  = newSex
                            , parentIds = [personId dad,personId mom]
                            , specialness = spec
                            }

    -- Flatten the [Maybe Person] down to [Person]
    return (catMaybes maybeBabies)

calculateSpecialness :: SimMonad Int
calculateSpecialness = do
    -- TODO: actual specialness logic
    return 0

-- Stateful helpers

freshId :: SimMonad PersonId
freshId = do
    (gen, nextId) <- get
    put (gen, nextId + 1)
    return nextId

rollDoubleRange :: (Double, Double) -> SimMonad Double
rollDoubleRange (low, high) = do
    (gen, nextId) <- get
    let (val, nextGen) = randomR (low, high) gen
    put (nextGen, nextId)
    return val

rollIntRange :: (Int, Int) -> SimMonad Int
rollIntRange (low, high) = do
    (gen, nextId) <- get
    let (val, nextGen) = randomR (low, high) gen
    put (nextGen, nextId)
    return val

rollListSelection :: [a] -> SimMonad a
rollListSelection xs = do
    let size = length xs
    index <- rollIntRange (0,size-1)
    return $ xs !! index

rollSex :: SimMonad Sex
rollSex = do
    roll <- rollDoubleRange (0.0,1.0)
    return (if roll < 0.5 then Male else Female)