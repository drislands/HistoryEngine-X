module Main where

import Control.Monad.State
import Data.List (find)
import Data.Maybe (mapMaybe)
import HistoryEngine.Logic
import HistoryEngine.Simulation
import HistoryEngine.Types
import System.IO (hFlush, stdout)
import System.Random (newStdGen)

main :: IO ()
main = do
    putBar
    putStrLn "History Engine REPL - Alpha"
    putBar

    -- Initialize random generator and initial empty World
    systemGen <- newStdGen
    let initialWorld = World{currentYear = 0, populations = [], historicalLedger = []}
        initialState = SimState{simGen = systemGen, nextPersonId = 1, simWorld = initialWorld}
        initialRepl = ReplState{activePopulationName = Nothing, activeSimState = initialState}

    replLoop initialRepl

-- REPL logic
data ReplState = ReplState
    { activePopulationName :: Maybe String
    , activeSimState :: SimState
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
                "select" : _ -> putStrLn "select <population name>"
                "update" : uRest -> do
                    case uRest of
                        [] -> putStrLn "Update one of: birth, mortality"
                        "birth" : _ -> putStrLn "update birth <new ratio>"
                        "mortality" : _ -> putStrLn "update mortality <new ratio>"
                        x : _ -> putStrLn $ "Unknown update argument `" ++ x ++ "`."
                "advance" : _ -> putStrLn "advance <years>"
                "examine" : _ -> putStrLn "examine <person ID>"
                x : _ -> putStrLn $ "Unknown command `" ++ x ++ "`."
            replLoop rState
        "create" : name : hcStr : sxStr : brStr : mrStr : _ -> do
            let headcount = read hcStr :: Int
                sRatio = read sxStr :: Ratio
                bRatio = read brStr :: Ratio
                mRatio = read mrStr :: Ratio

                simAction = generateInitialPopulation name headcount sRatio bRatio mRatio
                (newPop, nextState) = runState simAction (activeSimState rState)

                -- Append newPop to the world populations
                world = simWorld nextState
                updatedWorld = world{populations = populations world ++ [newPop]}
                finalState = nextState{simWorld = updatedWorld}

            putStrLn $ "Created population '" ++ name ++ "' with " ++ hcStr ++ " founders."
            replLoop rState{activePopulationName = Just name, activeSimState = finalState}
        "select" : name : _ -> do
            let world = simWorld (activeSimState rState)
                pops = populations world
            case findNamedPopulation name pops of
                Nothing -> do
                    putStrLn $ "Error: Population '" ++ name ++ "' does not exist!"
                    replLoop rState
                Just _ -> do
                    putStrLn $ "Selected active population '" ++ name ++ "'."
                    replLoop rState{activePopulationName = Just name}
        "update" : "birth" : bStr : _ -> do
            case activePopulationName rState of
                Nothing -> putStrLn "Error: Select or create a population first!" >> replLoop rState
                Just name -> do
                    let sState = activeSimState rState
                        world = simWorld sState
                        pops = populations world
                    case findNamedPopulation name pops of
                        Nothing -> putStrLn ("Error: Active population '" ++ name ++ "' not found!") >> replLoop rState
                        Just pop -> do
                            let newPop = pop{baseBirthRate = read bStr}
                                newPops = map (\p -> if popName p == name then newPop else p) pops
                                newWorld = world{populations = newPops}
                                newSimState = sState{simWorld = newWorld}
                            putStrLn $ "Updated base birth rate for population '" ++ name ++ "'."
                            replLoop rState{activeSimState = newSimState}
        "update" : "mortality" : mStr : _ -> do
            case activePopulationName rState of
                Nothing -> putStrLn "Error: Select or create a population first!" >> replLoop rState
                Just name -> do
                    let sState = activeSimState rState
                        world = simWorld sState
                        pops = populations world
                    case findNamedPopulation name pops of
                        Nothing -> putStrLn ("Error: Active population '" ++ name ++ "' not found!") >> replLoop rState
                        Just pop -> do
                            let newPop = pop{baseMortalityRate = read mStr}
                                newPops = map (\p -> if popName p == name then newPop else p) pops
                                newWorld = world{populations = newPops}
                                newSimState = sState{simWorld = newWorld}
                            putStrLn $ "Updated base mortality rate for population '" ++ name ++ "'."
                            replLoop rState{activeSimState = newSimState}
        "advance" : yearsStr : _ -> do
            let steps = read yearsStr :: Int
            if steps < 0
                then putStrLn "Error: cannot go back in time" >> replLoop rState
                else do
                    let sState = activeSimState rState
                        world = simWorld sState
                        pops = populations world
                    if null pops
                        then putStrLn "Error: Create a population first!" >> replLoop rState
                        else do
                            putStrLn $ "Advancing simulation by " ++ yearsStr ++ " years..."
                            let simAction = runWorldMultipleYears steps
                                (_, finalState) = runState simAction sState
                                finalWorld = simWorld finalState
                            mapM_ (printPopulationReport ("After " ++ yearsStr ++ " Years")) (populations finalWorld)
                            replLoop rState{activeSimState = finalState}
        "list" : _ -> do
            let sState = activeSimState rState
                world = simWorld sState
                pops = populations world
            if null pops
                then putStrLn "Error: No populations exist. Create one first!" >> replLoop rState
                else do
                    putStrLn "Populations in the world:"
                    mapM_
                        ( \pop -> do
                            let prefix = if Just (popName pop) == activePopulationName rState then " * " else "   "
                            putStrLn $ prefix ++ popName pop ++ " (Count: " ++ show (length (people pop)) ++ ")"
                        )
                        pops
                    replLoop rState
        "examine" : examineType : examinee : _ -> do
            case examineType of
                "person" -> do
                    let sState = activeSimState rState
                        world = simWorld sState
                        allPeople = concatMap (\pop -> people pop ++ deceased pop) (populations world)
                        pId = read examinee :: PersonId
                        person = findPerson pId allPeople
                    case person of
                        Nothing -> putStrLn ("Person with ID " ++ examinee ++ " does not exist!")
                        Just p -> do
                            let parents = mapMaybe (`findPerson` allPeople) (parentIds p)
                            putBar
                            putStrLn $ personName p
                            putLine
                            putStrLn $ "Sex: " ++ show (sex p)
                            putStrLn $ "Age: " ++ show (age p)
                            putStrLn $ "Parents: " ++ concatMap (\parent -> personName parent ++ " [" ++ show (personId parent) ++ "], ") parents
                            putBar
                    replLoop rState
                "population" -> do
                    let sState = activeSimState rState
                        world = simWorld sState
                        pop = findNamedPopulation examinee (populations world)
                    case pop of
                        Nothing -> putStrLn ("Population " ++ examinee ++ " does not exist!")
                        Just p -> do
                            printPopulationReport ("Year " ++ show (currentYear world)) p
                    replLoop rState
                _ -> do
                    putStrLn ("Unknown examination type: " ++ examineType)
                    replLoop rState
                
        _ -> do
            putStrLn "Unknown command or invalid arguments."
            help
            replLoop rState

-- Just a basic help message.
help :: IO ()
help = putStrLn "Available commands: help, create, select, update birth|mortality, advance, examine"

-- Some helper print functions
put20 :: Char -> IO ()
put20 c = putStrLn $ replicate 20 c

putBar :: IO ()
putBar = put20 '='

putLine :: IO ()
putLine = put20 '-'

-- Other helper functions
findNamedPopulation :: String -> [Population] -> Maybe Population
findNamedPopulation name = find (\p -> popName p == name)

-- Population Manipulation
generateInitialPopulation :: String -> Int -> Ratio -> Ratio -> Ratio -> SimMonad Population
generateInitialPopulation name popCount sexRatio birthRatio mortalityRatio = do
    -- Set gender populations
    let maleCount = round (fromIntegral popCount * sexRatio)
        femaleCount = popCount - maleCount

    -- Build the action that creates the actual people
    males <- replicateM maleCount (createStartingPerson Male)
    females <- replicateM femaleCount (createStartingPerson Female)

    return
        Population
            { popName = name
            , baseBirthRate = birthRatio
            , baseMortalityRate = mortalityRatio
            , people = males ++ females
            , deceased = []
            , discoveries = []
            }
  where
    createStartingPerson :: Sex -> SimMonad Person
    createStartingPerson personSex = do
        pId <- freshId
        -- TODO: age range ratios, too. For now we're just doing Adults
        startingAge <- rollIntRange (18, 49)
        return
            Person
                { personId = pId
                , personName = "Founder " ++ show pId
                , age = startingAge
                , sex = personSex
                , parentIds = []
                , specialness = 0
                }

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
    putStrLn $ "Total Population: " ++ show (length $ people pop)
    putLine
    mapM_ (\(ag, count) -> putStrLn $ " - " ++ show ag ++ "  | " ++ show count) ageCounts
    putLine
    mapM_ (\(thisSex, count) -> putStrLn $ " - " ++ show thisSex ++ " | " ++ show count) sexCounts
    putBar