module Day4.CeresSearch (solveDay4) where

import Data.List (transpose)
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))
import Util

solveDay4 input = do
    putStrLn "Day 4 - Ceres Search:"
    let inputLines = lines input
    print . part1 $ inputLines
    print . findCrosses $ inputLines

regexmas = "XMAS"

findInLine :: String -> Int
findInLine line = findForwardInLine line + findForwardInLine (reverse line)
  where
    findForwardInLine = length . getAllXmasMatches
    getAllXmasMatches :: String -> [String]
    getAllXmasMatches = getAllTextMatches . (=~ regexmas)

paddingChar = '_'
diagonalTranspose :: [String] -> [String]
diagonalTranspose lines = diagonalTransposeHelper [] (replicate (length lines) paddingChar) lines
  where
    diagonalTransposeHelper pre post@(x : xs) (l : ls) = (pre ++ l ++ post) : diagonalTransposeHelper (x : pre) xs ls
    diagonalTransposeHelper _ _ [] = []

allDiagonals :: [String] -> [String]
allDiagonals lines = tlbr lines ++ replicate (length . head . tlbr $ lines) paddingChar : trbl lines
  where
    trbl = diagonalTranspose
    tlbr = map reverse . diagonalTranspose . map reverse

findHorizontal = sum . map findInLine
findVertical = findHorizontal . transpose
findDiagonal = findVertical . allDiagonals

part1 = sum . zipWith ($) [findHorizontal, findVertical, findDiagonal] . repeat

findCrosses = sum . map countCrosses . windows 3
  where
    countCrosses [[], [], []] = 0
    countCrosses ls@[us@(a : _ : b : _), vs@(_ : 'A' : _ : _), ws@(c : _ : d : _)]
        | (a == 'M' && d == 'S' || a == 'S' && d == 'M') && (b == 'M' && c == 'S' || b == 'S' && c == 'M') = 1 + (countCrosses . map tail $ ls)
    countCrosses ls = countCrosses . map tail $ ls
