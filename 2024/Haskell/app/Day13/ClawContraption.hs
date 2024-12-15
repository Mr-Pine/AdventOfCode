module Day13.ClawContraption (solveDay13) where

import Data.Maybe (fromJust, mapMaybe)
import Text.Megaparsec (anySingle, sepEndBy1)
import Text.Megaparsec.Char (space, string)
import Util

solveDay13 input _ = do
    putStrLn "Day 13 - Claw Contraption:"
    arcade <- parseOrError arcadeParser input
    print . part1 $ arcade
    print . part2 $ arcade

part1 = sum . map (uncurry ((+) . (*3))) . mapMaybe solveLSE
part2 = sum . map (uncurry ((+) . (*3))) . mapMaybe (solveLSE . increaseResult (10000000000000, 10000000000000))

data Machine n = Machine {a :: (n, n), b :: (n, n), target :: (n, n)} deriving (Eq, Show)

increaseResult (dx, dy) (Machine a b (x, y)) = Machine a b (x + dx, y + dy)

solveLSE machine = takeIf (uncurry (verify machine)) . uncurry toInt . getAB . gauss . fromInteg $ machine
  where
    fromInteg (Machine (xa, ya) (xb, yb) (x, y)) = Machine (fromIntegral xa, fromIntegral ya) (fromIntegral xb, fromIntegral yb) (fromIntegral x, fromIntegral y)

    normalize (Machine (xa, ya) (xb, yb) (x, y)) = Machine (1, 1) (xb / xa, yb / ya) (x / xa, y / ya)
    gaussNormal (Machine (1, 1) (xb, yb) (x, y)) = Machine (1, 0) (xb, 1) (x, (y - x) / (yb - xb))
    gauss = gaussNormal . normalize

    getB (Machine (1, 0) (ya, 1) (x, y)) = y
    getA (Machine (1, 0) (ya, 1) (x, y)) = x - (ya * y)

    getAB m = (getA m, getB m)

    toInt a b = (round a, round b)
    verify (Machine (xa, ya) (xb, yb) (x, y)) a b = a * xa + b * xb == x && a * ya + b * yb == y

floatToInt :: (Integral i, RealFrac f) => f -> Maybe i
floatToInt = (round <$>) . takeIf (\f -> abs (f - fromIntegral (round f)) < 1e-5)

arcadeParser :: Parser [Machine Int]
arcadeParser = machineParser `sepEndBy1` space
  where
    machineParser = Machine <$> buttonParser <* space <*> buttonParser <* space <*> targetParser
    buttonParser = (,) <$> (string "Button " *> anySingle *> string ": X+" *> number) <*> (string ", Y+" *> number)
    targetParser = (,) <$> (string "Prize: X=" *> number) <*> (string ", Y=" *> number)
