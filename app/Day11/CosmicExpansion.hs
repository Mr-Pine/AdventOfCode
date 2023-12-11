{-# LANGUAGE TupleSections #-}

module Day11.CosmicExpansion (solveDay11) where

import Data.List (intercalate, transpose)
import Util (Prettify (prettify), debugMessage, debugMessagePlain, example, input)

solveDay11 = do
  putStrLn "Day 11 - Cosmic expansion"
  input <- input 11
  let image = map (map parsePixel) . lines $ input
  print . part1 $ image
  print . part2 $ image

part1 = solve 2
part2 = solve 1000000
solve expansionRatio = sum . map manhattanDistance . pairings . extractGalaxies . enumeratePixels expansionRatio

data Pixel = Emptyness | Galaxy deriving (Show, Eq)

parsePixel '#' = Galaxy
parsePixel '.' = Emptyness

instance Prettify Pixel where
  prettify Galaxy = "#"
  prettify Emptyness = "."

extractGalaxies :: [[(Pixel, (Int, Int))]] -> [(Int, Int)]
extractGalaxies = map snd . filter ((== Galaxy) . fst) . concat

pairings :: [a] -> [(a, a)]
pairings [] = []
pairings (x : xs) = map (x,) xs ++ pairings xs

enumeratePixels :: Int -> [[Pixel]] -> [[(Pixel, (Int, Int))]]
enumeratePixels expansionRatio pixels = enumerateRows 0 rowEmptyness pixels
    where
        getRowEmptyness :: [[Pixel]] -> [Bool]
        getRowEmptyness = map (all (==Emptyness))
        rowEmptyness = getRowEmptyness pixels
        columnEmptyness = getRowEmptyness . transpose $ pixels

        enumerateRows :: Int -> [Bool] -> [[Pixel]] -> [[(Pixel, (Int, Int))]]
        enumerateRows y (True:es) (r:rs) = enumerateColumns y 0 columnEmptyness r : enumerateRows (y + expansionRatio) es rs
        enumerateRows y (False:es) (r:rs) = enumerateColumns y 0 columnEmptyness r : enumerateRows (y + 1) es rs
        enumerateRows _ [] [] = []

        enumerateColumns :: Int -> Int -> [Bool] -> [Pixel] -> [(Pixel, (Int, Int))]
        enumerateColumns y x (True:es) (c:cs) = enumerateColumns y (x + expansionRatio) es cs
        enumerateColumns y x (False:es) (c:cs) = (c, (x,y)) : enumerateColumns y (x+1) es cs
        enumerateColumns _ _ [] [] = []

manhattanDistance ((x1, y1), (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)