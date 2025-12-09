module Day9.MovieTheater (solveDay9) where

import Data.List (sortBy, tails, uncons)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Text.Megaparsec (sepEndBy1, some, (<|>))
import Text.Megaparsec.Char (char, newline)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Util

solveDay9 input _ = do
    putStrLn "Day 9 - MovieTheater:"
    redTiles <- parseOrError redTileParser input
    print . part1 $ redTiles
    print . part2 $ redTiles

part1 = maximum . concatMap (uncurry (map . area)) . mapMaybe uncons . tails
part2 redTiles = head . map (uncurry area . fst) . filter (null . snd) . map (criticalLines . sortCoords) . sortBy (comparing (negate . uncurry area)) . concatMap (uncurry (map . (,))) . mapMaybe uncons . tails $ redTiles
  where
    sortCoords ((a,b),(x,y)) = ((min a x, min b y), (max a x, max b y))

    criticalLines cs = (cs, filter (lineCritical cs) greenLines)

    greenLines = map sortCoords $ (head redTiles, last redTiles) : map (\[a,b] -> (a,b)) (windows 2 redTiles)
    lineCritical ((c1X, c1Y), (c2X, c2Y)) (l1@(l1X, l1Y), l2@(l2X, l2Y)) = insideRect l1 || insideRect l2 || cutsHorizontal || cutsVertical where
        insideRect (x,y) = x > c1X && x < c2X && y > c1Y && y < c2Y
        cutsHorizontal = l1X > c1X && l2X < c2X && l1Y <= c1Y && l2Y >= c2Y
        cutsVertical = l1Y > c1Y && l2Y < c2Y && l1X <= c1X && l2X >= c2X

area (a, b) (x, y) = (xDist + 1) * (yDist + 1)
  where
    xDist = abs (a - x)
    yDist = abs (b - y)

redTileParser :: Parser [(Int, Int)]
redTileParser = ((,) <$> Lex.decimal <* char ',' <*> Lex.decimal) `sepEndBy1` newline
