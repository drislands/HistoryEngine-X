module Main where

import HistoryEngine.Types
import HistoryEngine.Logic
import HistoryEngine.Simulation

import System.Random (newStdGen)
import Control.Monad.State


main :: IO ()
main = do
    putStrLn "== Initializing Test Population =="

    -- 40 people, even gender split, 25% chance of birth, 10% chance of death
    testPop <- generateInitialPopulation "The Founders" 40 0.5 0.25 0.10

    printPopulationReport "Year 0" testPop

    putStrLn "\n== Advancing =="

    loopGen <- newStdGen
    let simState = (loopGen, 1000)

    let (advPop,_) = runState (advancePopulation testPop) simState

    printPopulationReport "Year 1" advPop

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
        sexes = [minBound .. maxBound] :: [Sex]
        ageCounts = map (\ag -> (ag, length [p | p <- people pop, ageGroup p == ag])) ages
        sexCounts = map (\thisSex -> (thisSex, length [p | p <- people pop, sex p == thisSex])) sexes


    putStrLn "==================="
    putStrLn $ label ++ " " ++ popName pop
    putStrLn "-------------------"
    putStrLn $ "Total Population: " ++ show ( length $ people pop)
    putStrLn "-------------------"
    mapM_ (\(ag, count) -> putStrLn $ " - " ++ show ag ++ " | " ++ show count) ageCounts
    putStrLn "-------------------"
    mapM_ (\(thisSex, count) -> putStrLn $ " - " ++ show thisSex ++ " | " ++ show count) sexCounts
    putStrLn "==================="