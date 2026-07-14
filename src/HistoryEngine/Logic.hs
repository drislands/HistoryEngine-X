module HistoryEngine.Logic where

import HistoryEngine.Types
import Data.List (find)
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

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

getAncestors :: Int -> PersonId -> Census -> Set.Set PersonId
getAncestors depth pid cns
    | depth < 0 = Set.empty
    | otherwise = case Map.lookup pid cns of
        Nothing -> Set.singleton pid
        Just p ->
            let parentTrees = [ getAncestors (depth - 1) parentId cns | parentId <- parentIds p]
            in Set.insert pid (Set.unions parentTrees)

areUnrelated :: PersonId -> PersonId -> Census -> Bool
areUnrelated id1 id2 cns =
    let tree1 = getAncestors 2 id1 cns -- self, parents, grandparents
        tree2 = getAncestors 2 id2 cns
    in Set.null (Set.intersection tree1 tree2)

-- Population functions
currentMortalityRate :: Population -> Ratio 
currentMortalityRate pop =
    let base = baseMortalityRate pop
        mods = sum [ mortalityMod d | d <- discoveries pop ]
    in  max 0.0 (base + mods) -- ensure mortality never goes below 0%

currentBirthRate :: Population -> Ratio 
currentBirthRate pop =
    let base = baseBirthRate pop
        mods = sum [ birthRateMod d | d <- discoveries pop ]
    in  max 0.0 (base + mods)

getPeople :: Census -> Population -> [Person]
getPeople pMap pop = mapMaybe (`Map.lookup` pMap) (people pop)

getDeceased :: Census -> Population -> [Person]
getDeceased pMap pop = mapMaybe (`Map.lookup` pMap) (deceased pop)

-- World functions
getBundleByYear :: Int -> World -> Maybe (BundledPopChange Year)
getBundleByYear year world =
    let ledger = historicalLedger world
    in find (\(_,y) -> y == year) ledger