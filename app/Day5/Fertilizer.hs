{-# LANGUAGE RecordWildCards #-}

module Day5.Fertilizer (solveDay5) where

import Util (example, parseOrError, Parser, debugMessage, input)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec (some, endBy, sepEndBy, sepBy)
import Text.Megaparsec.Char (alphaNumChar, string, newline, space, hspace)
import GHC.Data.Maybe (firstJusts, fromMaybe)

solveDay5 = do
  putStrLn "Day 5 - If You Give A Seed A Fertilizer:"
  input <- input 5
  almanac1 <- parseOrError (almanacParser seedsParserPart1) input
  almanac2 <- parseOrError (almanacParser seedsParserPart2) input
  print $ solve almanac1
  print $ solve almanac2
-- print $ part2 scratchcards

solve Almanac{..} = (minimum . map (`location` maps)) seeds

data Almanac = Almanac
  { seeds :: [Int],
    maps :: [Map]
  } deriving (Show)

data Mapping = Mapping
  { destinationStart :: Int,
    sourceStart :: Int,
    rangeLength :: Int
  } deriving (Show)

data Map = Map
  { from :: String,
    to :: String,
    mappings :: [Mapping]
  }
  deriving (Show)

almanacParser :: Parser [Int] -> Parser Almanac
almanacParser seedsParser = Almanac <$> seedsParser <* space <*>  mapsParser

seedsParserPart1 :: Parser [Int]
seedsParserPart1 = string "seeds:" *> space *> L.decimal `sepBy` hspace

seedsParserPart2 :: Parser [Int]
seedsParserPart2 = string "seeds:" *> space *> seedRanges
  where
    enumFromFor a b = [a .. a + b - 1]
    seedRange = enumFromFor <$>  L.decimal <* space <*> L.decimal :: Parser [Int]
    seedRanges = do
      ranges <- seedRange `sepBy` hspace
      let concated = concat ranges
      return concated


mapsParser :: Parser [Map]
mapsParser = mapParser `sepEndBy` space

mapParser :: Parser Map
mapParser = firstLine <*> mappingParser `sepEndBy` space
    where
        firstLine = Map <$> some alphaNumChar <* string "-to-" <*> some alphaNumChar <* string " map:" <* newline

mappingParser :: Parser Mapping
mappingParser = Mapping <$> number <*> number <*> number
    where number = L.lexeme space L.decimal

mapTo :: Int -> Mapping -> Maybe Int
mapTo n x@Mapping{..}
    | n >= sourceStart && n <= sourceStart + rangeLength = Just (destinationStart + (n - sourceStart))
    | otherwise = Nothing

transform :: Map -> Int -> Int
transform Map{..} n = (fromMaybe n . firstJusts . map (mapTo n)) mappings

location :: Int -> [Map] -> Int
location = foldl (flip transform)