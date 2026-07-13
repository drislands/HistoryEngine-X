module HistoryEngine.Simulation where

import HistoryEngine.Logic
import HistoryEngine.Types
import Data.Maybe (catMaybes)
import Control.Monad.State
import System.Random (StdGen, randomR)
import Data.Foldable (foldlM)


data SimState = SimState
    { simGen       :: StdGen
    , nextPersonId :: PersonId
    , simWorld     :: World
    } deriving (Show)

type SimMonad a = State SimState a


advanceWorld :: SimMonad ()
advanceWorld = do
    s <- get
    let world = simWorld s
    popChanges <- mapM advancePopulation (populations world)

    let deceasedIds = concatMap (pcDeaths . fst) popChanges
        babyIds     = concatMap (pcBirths . fst) popChanges
        newPops     = map snd popChanges
        newYear     = currentYear world + 1
        newBundle   = (PopulationChange{pcDeaths=deceasedIds,pcBirths=babyIds},currentYear world)
        oldLedger   = historicalLedger world
        newWorld    = world { populations = newPops, currentYear = newYear, historicalLedger = newBundle : oldLedger }

    put s { simWorld = newWorld }

runWorldMultipleYears :: Int -> SimMonad ()
runWorldMultipleYears 0 = return ()
runWorldMultipleYears n = advanceWorld >> runWorldMultipleYears (n - 1)

advancePopulation :: Population -> SimMonad (BundledPopChange Population)
advancePopulation pop = do
    -- Age em up
    let agedPeople = map (\p -> p { age = age p + 1 }) (people pop)

    -- Who dies?
    (survivingPeople,deadPeople) <- rollForDeath (currentMortalityRate pop) agedPeople
    -- TODO: keep the deceased in a different pool for the population...?

    -- Who is born?
    newBabies <- generateOffspring (currentBirthRate pop) survivingPeople

    -- Get the IDs we're bundling
    let deceasedIds = map personId deadPeople
        babyIds     = map personId newBabies
        popChange   = PopulationChange { pcDeaths = deceasedIds, pcBirths = babyIds }
        totalDead   = deadPeople ++ deceased pop

    -- Update the population with the survivors
    return (popChange, pop { people = survivingPeople ++ newBabies, deceased = totalDead })

-- Get a tuple containing the survivors and deceased, in that order.
rollForDeath :: Ratio  -> [Person] -> SimMonad ([Person],[Person])
rollForDeath rate = foldlM liveOrDie ([],[])
    where
        checkSurvival :: SimMonad Bool
        checkSurvival = do
            roll <- rollDoubleRange (0.0, 1.0)
            return (roll > rate)
        liveOrDie :: ([Person],[Person]) -> Person -> SimMonad ([Person],[Person])
        liveOrDie (living,dead) person = do
            survived <- checkSurvival
            if survived then return (person:living,dead)
            else return (living,person:dead)

generateOffspring :: Ratio  -> [Person] -> SimMonad [Person]
generateOffspring birthRate pool = do
    let females = [p | p <- pool, sex p == Female && canReproduce p]
        males   = [p | p <- pool, sex p == Male && canReproduce p]
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
                            , personName = "whatever"
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
    s <- get
    let nid = nextPersonId s
    put s { nextPersonId = nid + 1 }
    return nid

rollDoubleRange :: (Double, Double) -> SimMonad Double
rollDoubleRange (low, high) = do
    s <- get
    let (val, nextGen) = randomR (low, high) (simGen s)
    put s { simGen = nextGen }
    return val

rollIntRange :: (Int, Int) -> SimMonad Int
rollIntRange (low, high) = do
    s <- get
    let (val, nextGen) = randomR (low, high) (simGen s)
    put s { simGen = nextGen }
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