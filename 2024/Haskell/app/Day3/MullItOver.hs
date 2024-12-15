module Day3.MullItOver (solveDay3) where

import Data.List (singleton, sort)
import Data.List.Utils (countElem)
import Data.String.Utils (strip)
import Text.Megaparsec (MonadParsec (try), anySingle, many, manyTill, skipMany, skipManyTill, some)
import Text.Megaparsec.Char (char, newline, space, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Util

solveDay3 input _ = do
    putStrLn "Day 3 - Mull It Over:"
    multiplications <- parseOrError multiplicationsParser (strip input)
    print . part1 $ multiplications
    doDonts <- parseOrError doDontParser (strip input)
    filteredMultiplications <- parseOrError multiplicationsParser (concat doDonts)
    print . part1 $ filteredMultiplications

part1 = sum . map (uncurry (*))

multiplicationsParser :: Parser [(Int, Int)]
multiplicationsParser = some (try (skipManyTill anySingle (try multiplicationParser))) <* skipMany anySingle
  where
    multiplicationParser :: Parser (Int, Int)
    multiplicationParser = (,) <$> try (string "mul(" *> number <* char ',') <*> number <* char ')'

doDontParser :: Parser [String]
doDontParser = (++) <$> some (try (manyTill anySingle (try dontPart))) <*> (singleton <$> many anySingle)
  where
    dontPart :: Parser String
    dontPart = string "don't()" <* skipManyTill anySingle (string "do()")
