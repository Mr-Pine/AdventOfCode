module Day5.PrintQueue (solveDay5) where

import Data.List (sort, sortBy)
import Data.Maybe (fromJust)
import GHC.Data.Maybe (firstJusts)
import GHC.Utils.Misc (isSortedBy)
import Text.Megaparsec (MonadParsec (try), sepBy1, sepEndBy, some)
import Text.Megaparsec.Char (char, newline)
import Util
import Prelude

solveDay5 input _ = do
    putStrLn "Day 5 - Print Queue:"
    (orders, queues) <- parseOrError parser input
    print . part1 orders $ queues
    print . part2 orders $ queues

part1 orders = sum . map middleElement . filter (isSortedBy (combineRequirements orders))
part2 orders = sum . map (middleElement . sortBy (combineRequirements orders)) . filter (not . isSortedBy (combineRequirements orders))

orderParser :: Parser [(Int, Int)]
orderParser = try singleOrderParser `sepEndBy` newline
  where
    singleOrderParser :: Parser (Int, Int)
    singleOrderParser = (,) <$> number <*> (char '|' *> number)

queueParser :: Parser [[Int]]
queueParser = (number `sepBy1` char ',') `sepEndBy` newline

parser :: Parser ([(Int, Int)], [[Int]])
parser = (,) <$> orderParser <* some newline <*> queueParser

combineRequirements :: [(Int, Int)] -> Int -> Int -> Ordering
combineRequirements = combined
  where
    combined pairs l r = fromJust . firstJusts . map (($ r) . ($ l)) . partialOrderings $ pairs
    partialOrderings = map toOrder
    toOrder (a, b) x y
        | x == a && y == b = Just LT
        | x == b && y == a = Just GT
        | otherwise = Nothing

middleElement xs = xs !! (length xs `div` 2)
