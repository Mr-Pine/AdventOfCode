module Day3.Lobby (solveDay3) where

import Data.Char (digitToInt)
import Data.List (maximumBy, tails, uncons)
import Data.Array.IArray
import Data.Maybe (fromJust)
import Data.Tuple.HT (mapSnd)
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

part1 = sum . map bankValue
bankValue = maximum . map (uncurry ((. maximum) . (+) . (* 10)) . fromJust . uncons) . uncurry (filter . (. head) . (==) . maximum . map head) . dup . init . init . tails

part2 = sum . map (fromJust . (! (0, 12)) . (`joltData` 12))

joltData :: Bank -> Int -> Array (Int, Int) (Maybe Int)
joltData bank n = arr where
    arr = listArray bounds (map (uncurry joltValue) (range bounds))
    bankArray = listArray (0, length bank) bank :: Array Int Int
    multipliers = listArray (0, n) (scanl (const . (*10)) 1 [0..n - 1]) :: Array Int Int
    bounds = ((0,0), (length bank + 1, n))
    joltValue _ 0 = Just 0
    joltValue i _ | i == length bank = Nothing
    joltValue i n | n > length bank - i = Nothing
    joltValue i n = maximum . map (`joltValue'` n) $ [i..(length bank - 1)]

    joltValue' x n = (currentValue +) <$> subValue where
        batteryMultiplier = multipliers ! (n-1)
        batteryValue = bankArray ! x
        currentValue = batteryMultiplier * batteryValue
        subValue = arr ! (x + 1, n - 1)

type Bank = [Int]

systemParser :: Parser [Bank]
systemParser = bankParser `sepEndBy1` newline

bankParser :: Parser Bank
bankParser = some (digitToInt <$> digitChar)
