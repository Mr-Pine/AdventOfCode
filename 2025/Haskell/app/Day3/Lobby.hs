module Day3.Lobby (solveDay3) where

import Data.Char (digitToInt)
import Data.List (maximumBy, tails, uncons)
import Data.Array.IArray
import Data.Maybe (fromJust, catMaybes, mapMaybe)
import Data.Tuple.HT (mapSnd, mapFst)
import Data.Tuple.Utils (dup)
import Text.Megaparsec (sepEndBy1, some)
import Text.Megaparsec.Char (digitChar, newline)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Util
import Debug.Trace (traceShowId, traceShow)

solveDay3 input _ = do
    putStrLn "Day 3 - Lobby:"
    system <- parseOrError systemParser input
    print . part1 $ system
    print . part2 $ system

part1 = joltage 2
part2 = joltage 12

joltage = (sum .) . map . bankValue

bankValue 0 = const 0
bankValue n = uncurry (( . bankValue (n-1)) . (+)) . mapFst (* multiplier) . mapSnd snd . maximum . map (mapSnd (mapFst length . dup)) . mapMaybe uncons . dropLast n . tails
    where
        multiplier = foldl (const . (*10)) 1 [1..n-1]

type Bank = [Int]

systemParser :: Parser [Bank]
systemParser = bankParser `sepEndBy1` newline

bankParser :: Parser Bank
bankParser = some (digitToInt <$> digitChar)
