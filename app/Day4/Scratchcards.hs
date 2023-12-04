{-# LANGUAGE RecordWildCards #-}

module Day4.Scratchcards (solveDay4) where

import Data.List (intersect)
import Text.Megaparsec (endBy, sepBy, sepBy1)
import Text.Megaparsec.Char (char, hspace, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Util (Parser, debugMessage, debugMessageWith, example, input, parseOrError)

solveDay4 = do
  putStrLn "Day 4 - Gear Ratios:"
  input <- input 4
  scratchcards <- parseOrError scratchcardsParser input
  print $ part1 scratchcards
  print $ part2 scratchcards

part1 = sum . map value

part2 cards = countCards cards (replicate (length cards) 1)

data Scratchcard = Scratchcard
  { index :: Int,
    cardNumbers :: [Int],
    winningNumbers :: [Int]
  }
  deriving (Show)

value :: Scratchcard -> Int
value card = 2 `power` (wonAmount card - 1)

power x y
  | y < 0 = 0
  | otherwise = x ^ y

scratchcardsParser :: Parser [Scratchcard]
scratchcardsParser = scratchcardParser `sepBy1` newline

scratchcardParser :: Parser Scratchcard
scratchcardParser = Scratchcard <$> index <*> (decimal `endBy` space <* space <* char '|') <*> (space *> decimal `sepBy` hspace)
  where
    index = string "Card" *> space *> decimal <* char ':' <* space

countCards :: [Scratchcard] -> [Int] -> Int
countCards (x:xs) (c:cs) = c + countCards xs counts
  where
    counts = nextCounts ++ drop (wonAmount x) cs
    nextCounts = map (+c) (take (wonAmount x) cs)
countCards [] [] = 0

wonAmount Scratchcard {..} = length (winningNumbers `intersect` cardNumbers)