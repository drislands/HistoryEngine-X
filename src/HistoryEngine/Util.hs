module HistoryEngine.Util where

import Data.Char (isSpace)

tokenize :: String -> [String]
tokenize [] = []
tokenize (c:cs)
    | c == '"' = let (word, rest) = break (== '"') cs
                 in word : tokenize (if null rest then [] else tail rest)
    | isSpace c = tokenize cs
    | otherwise = let (word, rest) = break (\x -> isSpace x || c == '"') (c:cs)
                  in word : tokenize rest