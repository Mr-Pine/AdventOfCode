module Day1.HistorianHysteria (solveDay1) where
import Util
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space, newline)
import Text.Megaparsec (sepEndBy)
import Data.List (sort)
import Data.List.Utils (countElem)

solveDay1 input _ = do
  putStrLn "Day 1 - Historian Hysteria:"
  lists <- parseOrError listParser input
  print . part1 $ lists
  print . part2 $ lists

part1 :: [(Int, Int)] -> Int
part1 = sum . map (abs . uncurry (-)) . uncurry zip . mapPair sort . unzip

part2 :: [(Int, Int)] -> Int
part2 = uncurry part2Score . unzip
part2Score as bs = sum (zipWith (*) as (map (`countElem` bs) as))

lineParser :: Parser (Int, Int)
lineParser = (,) <$> L.decimal <* space <*> L.decimal

listParser :: Parser [(Int, Int)]
listParser = lineParser `sepEndBy` newline

mapPair f (x,y) = (f x, f y)
