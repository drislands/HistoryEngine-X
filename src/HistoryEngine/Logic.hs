module HistoryEngine.Logic where

import HistoryEngine.Types
import Data.List (find)
import qualified Data.Set as Set

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

findPerson :: PersonId -> [Person] -> Maybe Person
findPerson pid = find (\p -> personId p == pid)

getAncestors :: Int -> PersonId -> [Person] -> Set.Set PersonId
getAncestors depth pid pool
    | depth < 0 = Set.empty
    | otherwise = case findPerson pid pool of
        Nothing -> Set.singleton pid
        Just p ->
            let parentTrees = [ getAncestors (depth - 1) parentId pool | parentId <- parentIds p]
            in Set.insert pid (Set.unions parentTrees)

areUnrelated :: PersonId -> PersonId -> [Person] -> Bool
areUnrelated id1 id2 pool =
    let tree1 = getAncestors 2 id1 pool -- self, parents, grandparents
        tree2 = getAncestors 2 id2 pool
    in Set.null (Set.intersection tree1 tree2)

-- Population functions
currentMortalityRate :: Population -> Double
currentMortalityRate pop =
    let base = baseMortalityRate pop
        mods = sum [ mortalityMod d | d <- discoveries pop ]
    in  max 0.0 (base + mods) -- ensure mortality never goes below 0%

currentBirthRate :: Population -> Double
currentBirthRate pop =
    let base = baseBirthRate pop
        mods = sum [ birthRateMod d | d <- discoveries pop ]
    in  max 0.0 (base + mods)