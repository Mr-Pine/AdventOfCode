module Day22.MonkeyMarket (solveDay22) where

import Util
import Data.Bits (Bits(xor))
import GHC.Utils.Misc (nTimes)
import Data.List (tails)
import Data.Map (fromListWith, empty, unionWith, Map, lookupMax, (!?), unionsWith)

solveDay22 input _ = do
    putStrLn "Day 22 - Monkey Market:"
    print $ part1 input
    print $ part2 input

part1 = sum . map (nTimes 2000 stepRandom . read) . lines

part2 = maximum . unionsWith (+) . map (triggers . read) . lines
    where
        prices = take 2001 . iterate stepRandom
        triggers = fromListWith (flip const) . map (toTrigger . map (`mod` 10) . take 5) . filter ((>=5) . length) . tails . prices
        toTrigger xs = (zipWith (-) (drop 1 xs) xs,last xs)

stepRandom :: Int -> Int
stepRandom = step3 . step2 . step1
    where
        step1 n = prune $ n `mix` (n * 64)
        step2 n = prune $ n `mix` (n `div` 32)
        step3 n = prune $ n `mix` (n * 2048)

        mix = xor
        prune = (`mod` 16777216)
