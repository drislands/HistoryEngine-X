module HistoryEngine.Types where

import Data.Map


type PersonId = Int
type Year     = Int
type Ratio    = Double
type Census   = Map PersonId Person

data Sex = Male | Female
    deriving (Show, Eq, Enum, Bounded)

data AgeGroup = Child | Youth | Adult | Elder
    deriving (Show, Eq, Enum, Bounded)

data DiscoveryType = Technology | Magic | Culture
    deriving (Show, Eq)

data Discovery = Discovery
    { discoveryName :: String
    , discType      :: DiscoveryType
    , mortalityMod  :: Ratio       -- e.g., -0.05 (reduces mortality by 5%)
    , birthRateMod  :: Ratio       -- e.g., +0.02 (increases birth rate by 2%)
    , prerequisites :: [String]    -- Names of discoveries needed first
    } deriving (Show, Eq)

data Person = Person
    { personId    :: PersonId
    , personName  :: String
    , age         :: Int
    , sex         :: Sex
    , parentIds   :: [PersonId]      -- Ancestry tracking! Empty for the first generation
    , specialness :: Int
    } deriving (Show, Eq)

data Population = Population
    { popName           :: String
    , baseBirthRate     :: Ratio      -- Base percentage (e.g., 0.15)
    , baseMortalityRate :: Ratio      -- Base percentage (e.g., 0.10)
    , people            :: [PersonId]
    , deceased          :: [PersonId]
    , discoveries       :: [Discovery]
    } deriving (Show, Eq)

data PopulationChange = PopulationChange
    { pcBirths :: [PersonId]
    , pcDeaths :: [PersonId]
    } deriving (Show, Eq)

type BundledPopChange a = (PopulationChange, a)

data World = World
    { currentYear      :: Year
    , populations      :: [Population]
    , census           :: Census
    , historicalLedger :: [BundledPopChange Year]
    } deriving (Show, Eq)