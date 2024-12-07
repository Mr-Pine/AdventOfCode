module Day7.BridgeRepair (solveDay7) where

import Data.List (nub, singleton)
import Text.Megaparsec (sepBy1, sepEndBy1)
import Text.Megaparsec.Char (hspace, newline, string)
import Util

solveDay7 input = do
    putStrLn "Day 7 - Bridge Repair:"
    equations <- parseOrError equationsParser input
    print . part1 $ equations
    print . part2 $ equations

data Equation = Equation {target :: Int, values :: [Int]} deriving (Show, Eq)
equationSize = length . values

part1 = sumChecked operations1
part2 = sumChecked operations2

sumChecked ops = sum . map target . filter (check ops)

check ops (Equation target vals) = checkTargeted vals
  where
    checkTargeted [x] = target == x
    checkTargeted (x : y : zs)
        | x > target = False
        | otherwise = any (\o -> checkTargeted (x `o` y : zs)) ops

operations1 = [(+), (*)]
operations2 = concatOperator : operations1

concatOperator a b = a * 10 ^ (floor (logBase 10.0 (fromIntegral b)) + 1) + b

applyOperations (Equation target vals) = apply vals
  where
    apply [x] [] = x
    apply (x : y : zs) (o : os)
        | (x `o` y) <= target = apply (x `o` y : zs) os
        | otherwise = -1

equationParser :: Parser Equation
equationParser = Equation <$> number <* string ": " <*> (number `sepBy1` hspace)

equationsParser = equationParser `sepEndBy1` newline
