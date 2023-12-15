module Day15.LensLibrary (solveDay15) where
import Data.Char (ord)
import Data.List.Split (splitOn)
import Util (example, input)

solveDay15 = do
    putStrLn "Day 15 - Lens Library:"
    input <- input 15
    print . hash $ "HASH"
    print . part1 $ input

part1 = sum . map hash . steps

steps :: String -> [String]
steps = map (filter (/='\n')) . splitOn ","

hash :: String -> Int
hash = foldl nextHash 0
    where
        nextHash a c = (a + ord c) * 17 `rem` 256