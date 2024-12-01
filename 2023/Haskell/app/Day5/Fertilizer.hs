{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Day5.Fertilizer (solveDay5) where

import Util (example, parseOrError, Parser, debugMessage, input, debugMessageWith, Prettify, prettify)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec (some, endBy, sepEndBy, sepBy)
import Text.Megaparsec.Char (alphaNumChar, string, newline, space, hspace)
import GHC.Data.Maybe (firstJusts, fromMaybe)
import Data.List (singleton, sort, sortBy)
import Control.Parallel.Strategies (parMap, rdeepseq, rseq)

solveDay5 = do
  putStrLn "Day 5 - If You Give A Seed A Fertilizer:"
  input <- input 5
  almanac1 <- parseOrError (almanacParser seedsParserPart1) input
  almanac2 <- parseOrError (almanacParser seedsParserPart2) input

  print $ solve almanac1
  print $ solve almanac2

solve Almanac{..} = (minimum . parMap rdeepseq (minimum . map from . location maps . singleton)) seedRanges

data Range = Range
  { from :: Int,
    length :: Int
  } deriving (Show, Eq)

instance Ord Range where
  compare a b = compare a.from b.from

data Almanac = Almanac
  { seedRanges :: [Range],
    maps :: [Map]
  } deriving (Show)

data Mapping = Mapping
  { destination :: Range,
    source :: Range
  } deriving (Show)

data Map = Map
  { fromString :: String,
    toString :: String,
    mappings :: [Mapping]
  }
  deriving (Show)

almanacParser :: Parser [Range] -> Parser Almanac
almanacParser seedsParser = Almanac <$> seedsParser <* space <*>  mapsParser

seedsParserPart1 :: Parser [Range]
seedsParserPart1 = map singleRange <$> (string "seeds:" *> space *> L.decimal `sepBy` hspace)
  where
    singleRange x = Range x 1

seedsParserPart2 :: Parser [Range]
seedsParserPart2 = string "seeds:" *> space *> seedRange `sepBy` hspace
  where
    seedRange = Range <$>  L.decimal <* space <*> L.decimal :: Parser Range


mapsParser :: Parser [Map]
mapsParser = mapParser `sepEndBy` space

mapParser :: Parser Map
mapParser = firstLine <*> mappingParser `sepEndBy` space
    where
        sortMappings Map{..} = Map fromString toString (sortBy (\a b -> compare a.source b.source) mappings)
        sortedConstructor a b c = sortMappings (Map a b c)
        firstLine = sortedConstructor <$> some alphaNumChar <* string "-to-" <*> some alphaNumChar <* string " map:" <* newline

mappingParser :: Parser Mapping
mappingParser = mapping <$> number <*> number <*> number
  where
    number = L.lexeme space L.decimal
    mapping s d l = Mapping (Range s l) (Range d l)



transform :: Map -> [Range] -> [Range]
transform Map{..} = concatMap (filter (\range -> range.length > 0) . flip transformRanges mappings)

location :: [Map] -> [Range] -> [Range]
location m rs = foldl (flip transform) rs m

transformRanges :: Range -> [Mapping] -> [Range]
transformRanges seeds@Range{from = seedsFrom, length = seedsLength} (x@Mapping{..}:xs)
  | startInSource && endInSource = [Range startInDestination lengthOverlap]
  | startInSource = Range startInDestination lengthOverlap : transformRanges (Range sourceEnd (seedsLength - lengthOverlap)) xs
  | endInSource = [Range seedsFrom (seedsLength - lengthOverlap), Range startInDestination lengthOverlap]
  | lengthOverlap > 0 = Range seedsFrom (source.from - seedsFrom) : Range destination.from lengthOverlap : transformRanges (Range sourceEnd (seedsEnd - sourceEnd)) xs
  | otherwise = transformRanges seeds xs
  where
    seedsEnd = seedsFrom + seedsLength
    sourceEnd = source.from + source.length

    startInSource = seedsFrom >= source.from && seedsFrom < sourceEnd
    endInSource = seedsEnd <= sourceEnd && seedsEnd >= source.from

    lengthOverlap = min seedsEnd sourceEnd - max seedsFrom source.from

    startInDestination = (seedsFrom - source.from) + destination.from

transformRanges element [] = [element]