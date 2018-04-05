module Main where

import Network.HTTP
import Data.List (sortOn)
import Data.Map.Strict hiding (map)
import Data.FuzzySet

myfile = "http://www.gutenberg.org/cache/epub/51435/pg51435.txt"


main = do
     rsp <- Network.HTTP.simpleHTTP (getRequest myfile)
     content <- getResponseBody rsp
     -- print $ take 100 content
     let result = sortOn snd (toList $ Prelude.foldl insertWord empty (words content))
     print $ take 10 $ reverse result
     print $ take 10 result


insertWord :: Num f => Map String f -> String -> Map String f
insertWord map word = insertWith (+) word 1 map
