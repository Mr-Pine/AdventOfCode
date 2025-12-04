module Day4.PrintingDepartment (solveDay4) where

import Data.List (transpose)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Tuple.HT (mapSnd)
import Text.Megaparsec (sepEndBy1, some, (<|>))
import Text.Megaparsec.Char (char, newline)
import Util

solveDay4 input _ = do
    putStrLn "Day 4 - Printing Department:"
    grid <- parseOrError gridParser input
    print . day1 $ grid
    print . day2 $ grid

day1 = length . concatMap (filter id) . accessibilityMap

day2 grid = countRolls grid - countRolls allRemoved
  where
    countRolls = sum . concatMap (map value)

    allRemoved = fixpoint removeAccessible grid
    fixpoint f x
        | f x == x = x
        | otherwise = fixpoint f (f x)

removeAccessible grid = zipWith (zipWith remove) accessible grid
  where
    accessible = accessibilityMap grid

    remove False = id
    remove True = const Empty

accessibilityMap = map (map (maybe False ((< 4) . subtract 1) . rollValues . mapSnd countRolls . extractCell)) . windowCells . pad
  where
    rowWidth = length . head
    pad grid = map ((++ [Empty]) . (Empty :)) $ replicate (rowWidth grid) Empty : grid ++ [replicate (rowWidth grid) Empty]

    windowRows = windows 3
    windowCells = map (transpose . map (windows 3)) . windowRows

    extractCell b@[_, [_, x, _], _] = (x, b)
    countRolls = sum . concatMap (map value)

    rollValues (Roll, x) = Just x
    rollValues _ = Nothing

data GridCell = Empty | Roll deriving (Show, Eq)
value Empty = 0
value Roll = 1

gridParser :: Parser [[GridCell]]
gridParser = some ((Empty <$ char '.') <|> (Roll <$ char '@')) `sepEndBy1` newline
