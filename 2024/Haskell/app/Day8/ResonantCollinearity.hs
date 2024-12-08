{-# LANGUAGE TupleSections #-}

module Day8.ResonantCollinearity (solveDay8) where

import Data.List (nub, tails)
import Data.String.Utils (strip)
import Util

solveDay8 input = do
    putStrLn "Day 8 - Resonant Collinearity:"
    print . part1 $ input
    print . part2 $ input

part1 input = length . nub . concatMap (filter (inBounds (bounds input)) . uncurry toAntinodes) . pairings . antennas $ input
part2 input = length . nub . concatMap (filter (inBounds (bounds input)) . uncurry (toAllAntinodes (uncurry max (bounds input)))) . pairings . antennas $ input

bounds input = (length . head . lines $ input, length . lines . strip $ input)
antennas = concatMap (filter ((/= '.') . snd)) . xyEnumerate . lines . strip
pairings = concatMap (filter (uncurry ((. snd) . (==) . snd)) . (\ts -> map (head ts,) (tail ts))) . filter ((>= 2) . length) . tails
toAntinodes ((x1, y1), f1) ((x2, y2), f2) = [(x1 + (x1 - x2), y1 + (y1 - y2)), (x2 - (x1 - x2), y2 - (y1 - y2))]
toAllAntinodes expansion (a1@(x1, y1), _) ((x2, y2), _) = map (add a1 . flip mul step) [-expansion..expansion]
    where
        step = (x1 - x2, y1 - y2)
        reduceStep (x, y) = (x `div` gcd x y, y `div` gcd x y)
        reducedStep = reduceStep step
        add (x,y) (a,b) = (x+a, y+b)
        mul n (x,y) = (n * x, n * y)
