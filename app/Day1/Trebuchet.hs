module Day1.Trebuchet where
import Data.Char (isDigit)
import Data.List (findIndex, isPrefixOf)


solveDay1 = do
    input <- readFile "./input/1.input"
    let partOneData = map extractNumbers (lines input)
    let partTwoData = map (extractNumbers . transform) (lines input)
    putStrLn "Day 1 - Trebuchet?!:"
    print $ day1 partOneData
    print $ day1 partTwoData

day1 :: [String] -> Int
day1 = sum . map (read . (\numbers -> [head numbers, last numbers]))

numberStrings :: [String]
numberStrings = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

transform :: String -> String
transform (x:xs) | isDigit x = x : transform xs
transform l@(x:xs) = findNumber l ++ transform xs
transform [] = ""

findNumber :: String -> String
findNumber xs = maybe "" (show . (+1)) (getIndex xs)

getIndex :: String -> Maybe Int
getIndex xs = findIndex (`isPrefixOf` xs) numberStrings

extractNumbers :: String -> String
extractNumbers = filter isDigit