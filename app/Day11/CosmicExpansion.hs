{-# LANGUAGE TupleSections #-}

module Day11.CosmicExpansion (solveDay11) where

import Data.List (intercalate, transpose)
import Util (Prettify (prettify), debugMessage, debugMessagePlain, example, input)

solveDay11 = do
  putStrLn "Day 11 - Cosmic expansion"
  input <- input 11
  let image = map (map parsePixel) . lines $ input
  -- putStrLn . intercalate "\n" . map (intercalate "" . map prettify) . expand $ image
  print . part1 $ image

part1 = sum . map manhattanDistance . pairings . extractGalaxies . expand

data Pixel = Emptyness | Galaxy deriving (Show, Eq)

parsePixel '#' = Galaxy
parsePixel '.' = Emptyness

instance Prettify Pixel where
  prettify Galaxy = "#"
  prettify Emptyness = "."

expand :: [[Pixel]] -> [[Pixel]]
expand = transpose . expandRows . transpose . expandRows

expandRows :: [[Pixel]] -> [[Pixel]]
expandRows [] = []
expandRows (x : xs)
  | all (== Emptyness) x = x : x : expandRows xs
  | otherwise = x : expandRows xs

extractGalaxies :: [[Pixel]] -> [(Int, Int)]
extractGalaxies = map snd . filter ((== Galaxy) . fst) . concat . zipWith enumerateHorizontal [0 ..]
  where
    enumerateHorizontal i = zipWith (test i) [0 ..]
    test y x value = (value, (x, y))

pairings :: [a] -> [(a, a)]
pairings [] = []
pairings (x : xs) = map (x,) xs ++ pairings xs

manhattanDistance ((x1, y1), (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)