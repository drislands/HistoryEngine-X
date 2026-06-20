module Main where

import HistoryEngine.Types
import HistoryEngine.Logic
import HistoryEngine.Simulation

import System.Random (newStdGen)
import Control.Monad.State


main :: IO ()
main = do
    return ()

generateInitialPopulation :: String -> Int -> Ratio -> Ratio -> Ratio -> IO Population
generateInitialPopulation name popCount sexRatio birthRatio mortalityRatio = do
    -- Get a fresh random number generator from the system
    sysGen <- newStdGen

    -- Set up the initial state
    let initialState = (sysGen,1)

    -- Set gender populations
    let maleCount = round (fromIntegral popCount * sexRatio)
        femaleCount = popCount - maleCount
    
    -- Build the action that creates the actual people
    let generationAction = do
            males <- replicateM maleCount (createStartingPerson Male)
            females <- replicateM femaleCount (createStartingPerson Female)
            return (males ++ females)
    
    -- Run that action to make the population
    let (startingPeople, _) = runState generationAction initialState

    return Population
        { popName            = name
        , baseBirthRate      = birthRatio
        , baseMortalityRate  = mortalityRatio
        , people             = startingPeople
        , discoveries        = []
        }

    where
        createStartingPerson :: Sex -> SimMonad Person
        createStartingPerson personSex = do
            pId <- freshId
            -- TODO: age range ratios, too. For now we're just doing Adults
            startingAge <- rollIntRange (18, 49)
            return Person
                { personId    = pId
                , personName  = "Founder " ++ show pId
                , age         = startingAge
                , sex         = personSex
                , parentIds   = []
                , specialness = 0
                }

-- Print a visual representation of the population
printPopulationReport :: String -> Population -> IO ()
printPopulationReport label pop = do
    let ages = [minBound .. maxBound] :: [AgeGroup]
        ageCounts = map (\ag -> (ag, [p | p <- people pop, ageGroup p == ag])) ages


    putStrLn "==================="
    putStrLn $ label ++ " " ++ popName pop
    putStrLn "-------------------"
    mapM_ (\(ag, count) -> putStrLn $ show ag ++ " | " ++ show count) ageCounts
    putStrLn "==================="