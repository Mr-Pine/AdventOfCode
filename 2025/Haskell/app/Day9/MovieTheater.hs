module Day9.MovieTheater (solveDay9) where

import qualified Text.Megaparsec.Char.Lexer as Lex
import Util
import Text.Megaparsec (some, sepEndBy1, (<|>))
import Text.Megaparsec.Char (newline, char)
import Data.List (tails, uncons)
import Data.Maybe (mapMaybe)

solveDay9 input _ = do
    putStrLn "Day 9 - MovieTheater:"
    redTiles <- parseOrError redTileParser input
    print . part1 $ redTiles

part1 = maximum . concatMap (uncurry (map . area)) . mapMaybe uncons . tails

area (a,b) (x,y) = (xDist + 1) * (yDist + 1) where
    xDist = abs (a - x)
    yDist = abs (b - y)

redTileParser :: Parser [(Int,Int)]
redTileParser = ((,) <$> Lex.decimal <* char ',' <*> Lex.decimal) `sepEndBy1` newline
