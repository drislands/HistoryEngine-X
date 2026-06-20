module HistoryEngine.Types where


type PersonId = Int
type Year     = Int

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