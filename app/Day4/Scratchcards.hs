module Day4.Scratchcards (solveDay4) where

import Util (example, Parser, parseOrError, debugMessage, input)
import Text.Megaparsec.Char (string, space, char, newline, hspace)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec (sepBy, sepBy1, endBy)

solveDay4 = do
  putStrLn "Day 4 - Gear Ratios:"
  input <- example 4
  scratchcards <- parseOrError scratchcardsParser input
  print $ part1 scratchcards
  print $ part2 scratchcards

part1 = sum . map (debugMessage "Value " . value)
part2 cards = length (replaceCards cards cards)

data Scratchcard = Scratchcard {
    index :: Int,
    cardNumbers :: [Int],
    winningNumbers :: [Int]
} deriving Show

value :: Scratchcard -> Int
value card = 2 `power` (length (wonNumbers card) - 1)

power x y
    | y < 0 = 0
    | otherwise = x ^ y

scratchcardsParser :: Parser [Scratchcard]
scratchcardsParser = scratchcardParser `sepBy1` newline

scratchcardParser :: Parser Scratchcard
scratchcardParser = Scratchcard <$> index <*> (decimal `endBy` space <* space <* char '|') <*> (space *> decimal `sepBy` hspace)
  where
    index = string "Card" *> space *> decimal <* char ':' <* space

replaceCards :: [Scratchcard] -> [Scratchcard] -> [Scratchcard]
replaceCards full (x@Scratchcard{index = base}:xs) = x : additional ++ replaceCards full (additional ++ xs)
  where
    additional = (map ((full!!) . (+ base)) . debugMessage ("next Indices from Card " ++ show x ++ ": ") . getNextIndices) x
replaceCards _ [] = []

getNextIndices = enumFromTo 1 . length . wonNumbers

wonNumbers Scratchcard{cardNumbers = cardNumbers, winningNumbers = winningNumbers} = filter (`elem` winningNumbers) cardNumbers