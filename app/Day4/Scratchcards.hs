module Day4.Scratchcards (solveDay4) where

import Util (example, Parser, parseOrError, debugMessage, input)
import Text.Megaparsec.Char (string, space, char, newline, hspace)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec (sepBy, sepBy1, endBy)

solveDay4 = do
  putStrLn "Day 4 - Gear Ratios:"
  input <- input 4
  scratchcards <- parseOrError scratchcardsParser input
  print $ part1 scratchcards

part1 = sum . map (debugMessage "Value " . value)

data Scratchcard = Scratchcard {
    cardNumbers :: [Int],
    winningNumbers :: [Int]
} deriving Show

value :: Scratchcard -> Int
value Scratchcard {cardNumbers = cardNumbers, winningNumbers = winningNumbers} = 2 `power` ((length . filter (`elem` winningNumbers)) cardNumbers - 1)

power x y
    | y < 0 = 0
    | otherwise = x ^ y

scratchcardsParser :: Parser [Scratchcard]
scratchcardsParser = scratchcardParser `sepBy1` newline

scratchcardParser :: Parser Scratchcard
scratchcardParser = Scratchcard <$> (prefix *> decimal `endBy` space <* space <* char '|') <*> (space *> decimal `sepBy` hspace)
  where
    prefix = string "Card" *> space *> decimal *> char ':' *> space