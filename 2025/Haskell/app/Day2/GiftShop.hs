module Day2.GiftShop (solveDay2) where

import GHC.Num (integerLogBase)
import Text.Megaparsec (MonadParsec (eof), sepBy, sepBy1)
import Text.Megaparsec.Char (char, newline)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Util

solveDay2 input _ = do
    putStrLn "Day 2 - Secret Entrance:"
    ids <- parseOrError idsParser input
    print . part1 $ ids

data Id = Id {lower :: Integer, upper :: Integer} deriving (Show, Eq)

part1 = sum . map dupValue . concatMap validHalfs -- sum . concatMap invalidIds

-- invalidIds ids = filterRange ids . halfRanges $ ids

validHalfs idRange@(Id lower upper) = filterPossible idRange possibleHalfs
  where
    minDigitCount = (digitCount lower + 1) `div` 2 * 2 -- rounding up lower bound digit count to even
    minDigitValue = intExp 10 (minDigitCount - 1)
    maxDigitCount = digitCount upper `div` 2 * 2 -- rounding down lower bound digit count to even
    maxDigitValue = intExp 10 maxDigitCount - 1

    highRange = (fst (halfDigits (max lower minDigitValue)), fst (halfDigits (min upper maxDigitValue)))

    possibleHalfs = [fst highRange .. snd highRange]

filterPossible (Id lower upper) xs | length xs <= 2 = filter (invalidId . dupValue) xs
    where
        invalidId x = x `elem` [lower..upper]
filterPossible idRange (x:xs) = init xs ++ filterPossible idRange [x, last xs]

dupValue x = intExp 10 (digitCount x) * x + x

halfDigits x = (x `div` digitMask, x `mod` digitMask)
  where
    digitMask = intExp 10 (digitCount x `div` 2)

digitCount = (+1) . integerLogBase 10

intExp base n = foldr (const (*base)) 1 [1..n]

idsParser :: Parser [Id]
idsParser = idParser `sepBy1` char ',' <* newline <* eof

idParser :: Parser Id
idParser = Id <$> Lex.decimal <* char '-' <*> Lex.decimal
