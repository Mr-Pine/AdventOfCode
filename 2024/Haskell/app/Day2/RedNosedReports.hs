module Day2.RedNosedReports (solveDay2) where
import Util
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space, newline)
import Text.Megaparsec (sepEndBy)
import Data.List (sort)
import Data.List.Utils (countElem)
import GHC.Utils.Misc (split)
import Data.String.Utils (strip)

solveDay2 input _ = do
  putStrLn "Day 2 - Red-Nosed Reports:"
  let records = map (map read . split ' ') . split '\n' . strip $ input :: [Record]
  print . part1 $ records
  print . part2 $ records

part1 = length . filter validRecord
part2 = length . filter validDampenedRecord

type Record = [Int]

neighbours xs = zip xs (tail xs)

isMonotonicIncreasing :: Ord a => [a] -> Bool
isMonotonicIncreasing = all (uncurry (<)) . neighbours

isMonotonicDecreasing :: Ord a => [a] -> Bool
isMonotonicDecreasing = all (uncurry (>)) . neighbours

maxDifferenceOk :: Record -> Bool
maxDifferenceOk = (<= 3) . maximum . map (abs . uncurry (-)) . neighbours

validRecord r = maxDifferenceOk r && (isMonotonicIncreasing r || isMonotonicDecreasing r)

validDampenedRecord = any validRecord . dampenedRecords

dampenedRecords r = r : dampen [] r
    where
        dampen ls (r:rs) = (ls ++ rs) : dampen (ls ++ [r]) rs
        dampen ls [] = []
