{-# LANGUAGE RecordWildCards #-}

module Day6.WaitForIt (solveDay6) where

import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (digitChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Util (Parser, example, input, parseOrError)

solveDay6 = do
  input <- input 6
  races1 <- parseOrError (racesParser part1ListParser) input
  races2 <- parseOrError (racesParser part2ListParser) input
  print $ solve races1
  print $ solve races2

solve = product . map options

options race = upper race - lower race + 1

data Race = Race
  { duration :: Int,
    record :: Int
  }
  deriving (Show)

racesParser :: Parser [Int] -> Parser [Race]
racesParser intListParser = constructRaces <$> durations <*> records
  where
    durations = string "Time:" *> space *> intListParser :: Parser [Int]
    records = string "Distance:" *> space *> intListParser :: Parser [Int]
    constructRaces = zipWith Race

part1ListParser :: Parser [Int]
part1ListParser = L.decimal `sepEndBy` space

part2ListParser :: Parser [Int]
part2ListParser = toInt <$> digitChar `sepEndBy` space
  where
    toInt s = [read s]

lower r@Race {..} = floor (fromIntegral duration / 2 - discriminant r + 1)
upper r@Race {..} = ceiling (fromIntegral duration / 2 + discriminant r - 1)

discriminant Race {..} = sqrt (p ^ 2 / 4 - q)
  where
    p = fromIntegral duration
    q = fromIntegral record