{-# LANGUAGE TupleSections #-}

module Day11.PlutonianPebbles (solveDay11) where

import Control.Applicative ((<|>))
import Data.Map.Strict (Map, empty, insert, lookup, (!))
import Data.Maybe (fromJust)
import GHC.Data.Maybe (orElse)
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (space)
import Util
import Prelude hiding (lookup)

solveDay11 input = do
    putStrLn "Day 11 - Plutonian Pebbles:"
    stones <- parseOrError stonesParser input
    print . part1 $ stones
    print . part2 $ stones

part1 = countList 25
part2 = countList 75

countList steps = fst . foldr (addEvalStone steps) (0,empty)

addEvalStone steps stone (count,map) = (count + stoneCount, newMap)
    where
        (stoneCount,newMap) = countAfterSteps map steps stone

numDigits = (1 +) . floor . logBase 10 . fromIntegral

countAfterSteps :: Map (Int, Int) Int -> Int -> Int -> (Int, Map (Int, Int) Int)
countAfterSteps numbers 0 stone = ((,numbers) <$> (stone, 0) `lookup` numbers) `orElse` (1, insert (stone, 0) 1 numbers)
countAfterSteps numbers steps 0 = ((,numbers) <$> (0, steps) `lookup` numbers) `orElse` (oneCount, insert (0, steps) oneCount numbers)
  where
    (oneCount, oneMap) = countAfterSteps numbers (steps - 1) 1
countAfterSteps numbers steps stone
    | even (numDigits stone) = ((,numbers) <$> (stone, steps) `lookup` numbers) `orElse` res
  where
    (firstNum, secondNum) = split stone
    (count1, map1) = ((,numbers) <$> (firstNum, steps - 1) `lookup` numbers) `orElse` countAfterSteps numbers (steps - 1) firstNum
    (count2, map2) = ((,map1) <$> (secondNum, steps - 1) `lookup` map1) `orElse` countAfterSteps map1 (steps - 1) secondNum
    res = (count1 + count2, insert (stone, steps) (count1 + count2) map2)
countAfterSteps numbers steps stone = ((,numbers) <$> (stone, steps) `lookup` numbers) `orElse` res
  where
    (multCount, multMap) = countAfterSteps numbers (steps - 1) (stone * 2024)
    res = (multCount, insert (stone, steps) multCount multMap)

split x = (x `div` dividend, x `mod` dividend)
  where
    dividend = 10 ^ (numDigits x `div` 2)

stonesParser :: Parser [Int]
stonesParser = number `sepEndBy1` space
