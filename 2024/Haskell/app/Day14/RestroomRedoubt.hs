module Day14.RestroomRedoubt (solveDay14) where

import Data.Array (Ix (inRange))
import Data.List (intercalate, sort)
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (char, space, string)
import Util hiding (inBounds)

solveDay14 input isExample = do
    putStrLn "Day 14 - Restroom Redoubt:"
    robots <- parseOrError robotsParser input
    let bounds = if isExample then (11, 7) else (101, 103)
    print . part1 bounds $ robots
    print . part2 bounds $ robots
    -- mapM_ (putStrLn . (++ "\n") . displayGrid bounds) . filter (hasContinuousHorizontalLine 21 (snd bounds)) . map (\n -> map (step n bounds) robots) $ [1 .. 10000]

displayGrid (x, y) robots = intercalate "\n" . map (map posToChr) $ grid
  where
    grid = map (map fst) . xyEnumerate . replicate y . replicate x $ ()
    posToChr p = if any ((== p) . position) robots then '#' else '.'

part1 bounds@(x, y) robots = product . map length $ robotsInBounds
  where
    topLeft = ((0, 0), (x `div` 2 - 1, y `div` 2 - 1))
    topRight = ((x `div` 2 + 1, 0), (x - 1, y `div` 2 - 1))
    bottomLeft = ((0, y `div` 2 + 1), (x `div` 2 - 1, y - 1))
    bottomRight = ((x `div` 2 + 1, y `div` 2 + 1), (x - 1, y - 1))
    quadrandBounds = [topLeft, topRight, bottomLeft, bottomRight]

    robotsInBounds = map ((`filter` hundredStepRobots) . inBounds) quadrandBounds

    hundredStepRobots = map (step 100 bounds) robots

part2 bounds robots = head . filter isViableTree $ [1..]
  where
    isViableTree n = hasContinuousHorizontalLine 21 (snd bounds) (map (step n bounds) robots)

hasContinuousHorizontalLine n maxY robots = any containsHorizontalLine [0 .. maxY]
  where
    containsHorizontalLine y = any ((>= (n - 1)) . length) . filter ((== 1) . head) . runs [] . deltas . sort . map (fst . position) . filter ((== y) . snd . position) $ robots
    deltas xs = zipWith (-) (drop 1 xs) xs
    runs x []
        | null x = []
        | otherwise = [x]
    runs [] (x : xs) = runs [x] xs
    runs ass@(a : as) (x : xs)
        | x == a = runs (x : ass) xs
        | otherwise = ass : runs [x] xs

data Robot = Robot {position :: (Int, Int), velocity :: (Int, Int)} deriving (Eq, Show)

step n (boundX, boundY) (Robot (x, y) v@(dx, dy)) = Robot ((x + n * dx) `mod` boundX, (y + n * dy) `mod` boundY) v
inBounds = (. position) . inRange

robotsParser :: Parser [Robot]
robotsParser = robotParser `sepEndBy1` space
  where
    robotParser = Robot <$> (string "p=" *> coordinateParser) <*> (string " v=" *> coordinateParser)
    coordinateParser = (,) <$> number <* char ',' <*> number
