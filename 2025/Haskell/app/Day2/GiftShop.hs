{-# LANGUAGE DuplicateRecordFields #-}

module Day2.GiftShop (solveDay2) where

import Data.List.Utils (uniq)
import Data.Maybe (fromMaybe)
import GHC.Num (integerLogBase)
import Safe (headMay)
import Text.Megaparsec (MonadParsec (eof), sepBy, sepBy1)
import Text.Megaparsec.Char (char, newline)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Util

solveDay2 input _ = do
    putStrLn "Day 2 - Secret Entrance:"
    ids <- parseOrError idsParser input
    print . part1 $ ids
    print . part2 $ ids

data Id = Id {lower :: Integer, upper :: Integer} deriving (Show, Eq)

data IdData = IdData {lower :: Integer, upper :: Integer, lowerDigitCount :: Word, upperDigitCount :: Word} deriving (Show)

populate (Id lower upper) = IdData lower upper (digitCount lower) (digitCount upper)

part1 = sum . nPartsInvalid 2 . map populate
part2 idRanges = sum . uniq . concatMap partSums $ [2 .. maxN]
  where
    partSums n = nPartsInvalid n populated
    populated = map populate idRanges
    maxN = maximum . map upperDigitCount $ populated

nPartsInvalid n = map (replValue n) . concatMap (validParts n)

validParts n idRange@(IdData lower upper lowerDigitCount upperDigitCount) = filterPossible n idRange possibleParts
  where
    minDigitCount = (lowerDigitCount + (n - 1)) `div` n * n -- rounding up lower bound digit count to even
    minDigitValue = intExp 10 (minDigitCount - 1)
    maxDigitCount = upperDigitCount `div` n * n -- rounding down lower bound digit count to even
    maxDigitValue = intExp 10 maxDigitCount - 1

    highRange = (head (digitParts n (max lower minDigitValue)), fromMaybe 0 $ headMay (digitParts n (min upper maxDigitValue)))

    possibleParts = [fst highRange .. snd highRange]

filterPossible n (IdData lower upper _ _) xs | length xs <= 2 = filter (invalidId . replValue n) xs
  where
    invalidId x = x `elem` [lower .. upper]
filterPossible n idRange (x : xs) = init xs ++ filterPossible n idRange [x, last xs]

replValue n x = replValue' (intExp 10 (digitCount x)) n x
  where
    replValue' _ 1 x = x
    replValue' m n x = m * replValue' m (n - 1) x + x

digitParts n x = reverse . digitParts' $ x
  where
    digitMask = intExp 10 (digitCount x `div` n)
    digitParts' 0 = []
    digitParts' x = (x `mod` digitMask) : digitParts' (x `div` digitMask)

digitCount = (+ 1) . integerLogBase 10

intExp base n = foldr (const (* base)) 1 [1 .. n]

idsParser :: Parser [Id]
idsParser = idParser `sepBy1` char ',' <* newline <* eof

idParser :: Parser Id
idParser = Id <$> Lex.decimal <* char '-' <*> Lex.decimal
