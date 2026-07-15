{-# OPTIONS_GHC -Wno-unused-imports #-}
module HistoryEngine.Simulation where

import HistoryEngine.Logic
import HistoryEngine.Types
import Data.Maybe (catMaybes)
import Data.List (find)
import Control.Monad.State
import Control.Monad (forM)
import System.Random (StdGen, randomR)
import Data.Foldable (foldlM)
import qualified Data.Map as Map


data SimState = SimState
    { simGen       :: StdGen
    , nextPersonId :: PersonId
    , simWorld     :: World
    } deriving (Show)

type SimMonad a = State SimState a


advanceWorld :: SimMonad ()
advanceWorld = do
    s <- get
    let world      = simWorld s
        cns        = census world
        -- Collect all IDs of living people
        livingIds  = concatMap people (populations world)
        -- Age them up in the Census
        agedCensus = foldr (Map.adjust (\p -> p { age = age p + 1 })) cns livingIds
        agedWorld  = world { census = agedCensus }
    
    put s { simWorld = agedWorld }
        
    popChanges <- mapM advancePopulation (populations agedWorld)

    -- Getting the state again, since advancePopulation may have added newborns
    s' <- get
    let updatedWorld = simWorld s'
        deceasedIds  = concatMap (pcDeaths . fst) popChanges
        babyIds      = concatMap (pcBirths . fst) popChanges
        newPops      = map snd popChanges
        newYear      = currentYear updatedWorld + 1
        newBundle    = (PopulationChange{pcDeaths=deceasedIds,pcBirths=babyIds}, currentYear updatedWorld)
        oldLedger    = historicalLedger updatedWorld
        newWorld     = updatedWorld 
            { populations      = newPops
            , currentYear      = newYear
            , historicalLedger = newBundle : oldLedger 
            }

    put s' { simWorld = newWorld }

runWorldMultipleYears :: Int -> SimMonad ()
runWorldMultipleYears 0 = return ()
runWorldMultipleYears n = advanceWorld >> runWorldMultipleYears (n - 1)

advancePopulation :: Population -> SimMonad (BundledPopChange Population)
advancePopulation pop = do
    sState <- get
    let world        = simWorld sState
        cns          = census world
        livingPeople = getPeople cns pop

    -- Who dies?
    (survivingPeople,deadPeople) <- rollForDeath pop livingPeople
    -- TODO: keep the deceased in a different pool for the population...?

    -- Who is born?
    newBabies <- generateOffspring (currentBirthRate pop) survivingPeople

    -- Get the IDs we're bundling
    let deceasedIds  = map personId deadPeople
        babyIds      = map personId newBabies
        popChange    = PopulationChange { pcDeaths = deceasedIds, pcBirths = babyIds }
        newPeopleIds = map personId survivingPeople ++ babyIds
        newDeadIds   = deceasedIds ++ deceased pop
    
    -- Put the new babies into the census
    let updatedCensus = foldr (\p m -> Map.insert (personId p) p m) cns newBabies
        newWorld      = world { census = updatedCensus }
    put sState { simWorld = newWorld }

    -- Update the population with the survivors
    return (popChange, pop { people = newPeopleIds, deceased = newDeadIds })

-- Get a tuple containing the survivors and deceased, in that order.
rollForDeath :: Population -> [Person] -> SimMonad ([Person],[Person])
rollForDeath pop = foldlM liveOrDie ([],[])
    where
        baseRate = currentMortalityRate pop

        checkSurvival :: Person -> SimMonad Bool
        checkSurvival person = do
            roll <- rollDoubleRange (0.0, 1.0)

            -- Calculate an age penalty.
            -- TODO: Make the threshold and penalty configurable
            let ageMod = if age person > 50
                         then fromIntegral (age person - 50) * 0.015
                         else 0.0
                -- Make sure it doesn't go over 100%!
                rate = min 1.0 (baseRate + ageMod)

            return (roll > rate)
        liveOrDie :: ([Person],[Person]) -> Person -> SimMonad ([Person],[Person])
        liveOrDie (living,dead) person = do
            survived <- checkSurvival person
            if survived then return (person:living,dead)
            else return (living,person:dead)

generateOffspring :: Ratio  -> [Person] -> SimMonad [Person]
generateOffspring birthRate pool = do
    sState <- get
    let world = simWorld sState
        cns = census world

    let females = [p | p <- pool, sex p == Female && canReproduce p]
        males   = [p | p <- pool, sex p == Male && canReproduce p]
    maybeBabies <- forM females $ \mom -> do
        if null males
            then return Nothing
            else do
                birthRoll <- rollDoubleRange (0.0, 1.0)
                if birthRoll > birthRate
                    then return Nothing
                    else do
                        maybeDad <- findUnrelatedMale mom cns males 5
                        case maybeDad of
                            Nothing -> return Nothing
                            Just dad -> do
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

findUnrelatedMale :: Person -> Census -> [Person] -> Int -> SimMonad (Maybe Person)
findUnrelatedMale _ _ [] _ = return Nothing
findUnrelatedMale mom cns eligibleDads attempts = do
    maybeUnrelated <- tryRandomUnrelated mom cns eligibleDads attempts
    case maybeUnrelated of
        Just dad -> return (Just dad)
        Nothing -> do
            -- Fallback 1: Exhaustive search for any unrelated male
            let unrelatedDad = find (\dad -> areUnrelated (personId mom) (personId dad) cns) eligibleDads
            case unrelatedDad of
                Just dad -> return (Just dad)
                Nothing ->
                    -- Fallback 2: If absolutely no unrelated males exist, pick any eligible male
                    Just <$> rollListSelection eligibleDads

tryRandomUnrelated :: Person -> Census -> [Person] -> Int -> SimMonad (Maybe Person)
tryRandomUnrelated _ _ [] _ = return Nothing
tryRandomUnrelated mom cns eligibleDads attempts
    | attempts <= 0 = return Nothing
    | otherwise = do
        candidate <- rollListSelection eligibleDads
        if areUnrelated (personId mom) (personId candidate) cns
            then return (Just candidate)
            else tryRandomUnrelated mom cns eligibleDads (attempts - 1)

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
