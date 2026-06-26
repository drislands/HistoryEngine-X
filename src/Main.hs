module Main where

import HistoryEngine.Types
import HistoryEngine.Logic
import HistoryEngine.Simulation

import System.Random (newStdGen)
import Control.Monad.State
import System.IO (hFlush, stdout)
import Data.Maybe (mapMaybe)


main :: IO ()
main = do
    putBar
    putStrLn "History Engine REPL - Alpha"
    putBar

    -- Initialize random generator
    systemGen <- newStdGen
    let initialState = (systemGen, 1)
        initialRepl  = ReplState { activePopulation = Nothing, activeSimState = initialState }
    
    replLoop initialRepl

-- REPL logic
data ReplState = ReplState
    { activePopulation :: Maybe Population
    , activeSimState   :: SimState
    }

replLoop :: ReplState -> IO ()
replLoop rState = do
    putStr "he-x> "
    hFlush stdout -- forces prompt to display immediately
    input <- getLine

    let commandTokens = words input
    case commandTokens of
        [] -> replLoop rState
        ["exit"] -> putStrLn "Exiting."
        "help" : hRest -> do
            case hRest of
                [] -> help
                "create" : _ -> putStrLn "create <name> <headcount> <sex ratio> <birth ratio> <mortality ratio>"
                "update" : uRest -> do
                    case uRest of
                        [] -> putStrLn "Update one of: birth"
                        "birth" : _ -> putStrLn "update birth <new ratio>"
                        "mortality" : _ -> putStrLn "update mortality <new ratio>"
                        x : _ -> putStrLn $ "Unknown update argument `" ++ x ++ "`."
                "advance" : _ -> putStrLn "advance <years>"
                "examine" : _ -> putStrLn "examine <person ID>"
                x : _ -> putStrLn $ "Unknown command `" ++ x ++ "`."
            replLoop rState
        "create" : name : hcStr : sxStr : brStr : mrStr : _ -> do
            let headcount = read hcStr :: Int
                sRatio    = read sxStr :: Ratio
                bRatio    = read brStr :: Ratio
                mRatio    = read mrStr :: Ratio

                -- Pass the state in, carry it back out
                simAction = generateInitialPopulation name headcount sRatio bRatio mRatio
                (newPop, nextState) = runState simAction (activeSimState rState)
            putStrLn $ "Created population '" ++ name ++ "' with " ++ hcStr ++ " founders."

            -- Population created, start the loop again with this as the single pop
            replLoop rState { activePopulation = Just newPop, activeSimState = nextState}
        "update" : "birth" : bStr : _ -> do
            case activePopulation rState of
                Nothing -> putStrLn "Error: Create a population first!" >> replLoop rState
                Just pop -> do
                    let newPop  = pop { baseBirthRate = read bStr }
                    putStrLn "Updated base birth rate."
                    replLoop rState { activePopulation = Just newPop }
        "update" : "mortality" : mStr : _ -> do
            case activePopulation rState of
                Nothing -> putStrLn "Error: Create a population first!" >> replLoop rState
                Just pop -> do
                    let newPop  = pop { baseMortalityRate = read mStr }
                    putStrLn "Updated base mortality rate."
                    replLoop rState { activePopulation = Just newPop }
        -- the meat and potatoes.
        "advance" : yearsStr : _ ->
            case activePopulation rState of
                Nothing -> putStrLn "Error: Create a population first!" >> replLoop rState
                Just pop -> do
                    let steps = read yearsStr :: Int
                    if steps < 0 then putStrLn "Error: cannot go back in time" >> replLoop rState
                    else do
                        putStrLn $ "Advancing simulation by " ++ yearsStr ++ " years..."

                        let simAction = runMultipleYears steps pop
                            (finalPop,finalState) = runState simAction (activeSimState rState)
                        
                        printPopulationReport ("After " ++ yearsStr ++ " Years") finalPop
                        replLoop rState { activePopulation = Just finalPop, activeSimState = finalState }
        "list" : _ -> do
            case activePopulation rState of
                Nothing -> putStrLn "Error: Create a population first!" >> replLoop rState
                Just pop -> do
                    putStrLn $ "Here are all person IDs: " ++ show (people pop)
                    replLoop rState
        "examine" : pIdStr : _ -> do
            case activePopulation rState of
                Nothing -> putStrLn "Error: Create a population first!" >> replLoop rState
                Just pop -> do
                    let pId = read pIdStr :: PersonId
                        person = findPerson pId (people pop)
                    
                    case person of
                        Nothing -> putStrLn ("Person with ID " ++ pIdStr ++ " does not exist!")
                        Just p -> do
                            let parents = mapMaybe (\parId -> findPerson parId (people pop)) (parentIds p)
                            putBar
                            putStrLn $ personName p
                            putLine
                            putStrLn $ "Sex: " ++ show (sex p)
                            putStrLn $ "Age: " ++ show (age p)
                            putStrLn $ "Parents: " ++ concatMap (\parent -> personName parent ++ " [" ++ show (personId parent) ++ "], " ) parents
                            putBar

                    replLoop rState

        _ -> do
            putStrLn "Unknown command or invalid arguments."
            help
            replLoop rState

-- Just a basic help message.
help :: IO ()
help = putStrLn "Available commands: help, create, update birth|mortality, advance, examine"

-- Some helper print functions
put20 :: Char -> IO ()
put20 c = putStrLn $ replicate 20 c

putBar :: IO ()
putBar = put20 '='

putLine :: IO ()
putLine = put20 '-'

-- Population Manipulation
generateInitialPopulation :: String -> Int -> Ratio -> Ratio -> Ratio -> SimMonad Population
generateInitialPopulation name popCount sexRatio birthRatio mortalityRatio = do
    -- Set gender populations
    let maleCount = round (fromIntegral popCount * sexRatio)
        femaleCount = popCount - maleCount
    
    -- Build the action that creates the actual people
    males <- replicateM maleCount (createStartingPerson Male)
    females <- replicateM femaleCount (createStartingPerson Female)
    

    return Population
        { popName            = name
        , baseBirthRate      = birthRatio
        , baseMortalityRate  = mortalityRatio
        , people             = males ++ females
        , deceased           = []
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

runMultipleYears :: Int -> Population -> SimMonad Population
runMultipleYears 0 pop = return pop
runMultipleYears n pop = do
    nextPop <- advancePopulation pop
    runMultipleYears (n - 1) (snd nextPop) -- TODO: Do the real stuff

-- Print a visual representation of the population
printPopulationReport :: String -> Population -> IO ()
printPopulationReport label pop = do
    let ages = [minBound .. maxBound] :: [AgeGroup]
        sexes = [minBound .. maxBound] :: [Sex]
        ageCounts = map (\ag -> (ag, length [p | p <- people pop, ageGroup p == ag])) ages
        sexCounts = map (\thisSex -> (thisSex, length [p | p <- people pop, sex p == thisSex])) sexes


    putBar
    putStrLn $ label ++ " " ++ popName pop
    putLine
    putStrLn $ "Total Population: " ++ show ( length $ people pop)
    putLine
    mapM_ (\(ag, count) -> putStrLn $ " - " ++ show ag ++ "  | " ++ show count) ageCounts
    putLine
    mapM_ (\(thisSex, count) -> putStrLn $ " - " ++ show thisSex ++ " | " ++ show count) sexCounts
    putBar