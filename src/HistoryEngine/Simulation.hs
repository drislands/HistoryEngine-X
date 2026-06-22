module HistoryEngine.Simulation where

import HistoryEngine.Logic
import HistoryEngine.Types
import Data.Maybe (catMaybes)
import Control.Monad.State
import System.Random (StdGen, randomR)


type SimState = (StdGen, PersonId)

type SimMonad a = State SimState a

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

rollForDeath :: Ratio  -> [Person] -> SimMonad [Person]
rollForDeath rate = filterM checkSurvival
    where
        checkSurvival :: Person -> SimMonad Bool
        checkSurvival _ = do
            roll <- rollDoubleRange (0.0, 1.0)
            return (roll > rate)

generateOffspring :: Ratio  -> [Person] -> SimMonad [Person]
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