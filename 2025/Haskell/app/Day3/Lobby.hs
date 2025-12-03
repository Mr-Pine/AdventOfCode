module Day3.Lobby (solveDay3) where

import Util
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec.Char (digitChar, newline)
import Data.Char (digitToInt)
import Text.Megaparsec (some, sepEndBy1)
import Data.List (tails, maximumBy, uncons)
import Data.Tuple.Utils (dup)
import Data.Maybe (fromJust)
import Data.Tuple.HT (mapSnd)

solveDay3 input _ = do
    putStrLn "Day 3 - Lobby:"
    system <- parseOrError systemParser input
    print . part1 $ system

part1 = sum . map bankValue
bankValue = maximum . map (uncurry ((. maximum) . (+) . (*10)) . fromJust . uncons) . uncurry (filter . (. head) . (==) . maximum . map head) . dup . init . init . tails

type Bank = [Int]

systemParser :: Parser [Bank]
systemParser = bankParser `sepEndBy1` newline

bankParser :: Parser Bank
bankParser = some (digitToInt <$> digitChar)
