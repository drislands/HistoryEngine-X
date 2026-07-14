module HistoryEngine.Util where

import Data.Char (isSpace)

tokenize :: String -> [String]
tokenize [] = []
tokenize (c:cs)
    | isDQuote c = let (word, rest) = break isDQuote cs
                 in word : tokenize (if null rest then [] else tail rest)
    | isSpace c = tokenize cs
    | otherwise = let (word, rest) = break (\x -> isSpace x || isDQuote x) (c:cs)
                  in word : tokenize rest
        where
            isDQuote :: Char -> Bool
            isDQuote = (== '"')