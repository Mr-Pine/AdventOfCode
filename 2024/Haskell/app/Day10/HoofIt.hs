module Day10.HoofIt (solveDay10) where

import Data.Array (Array, array, assocs, bounds, inRange, listArray, (!))
import Data.Char (digitToInt)
import Data.String.Utils (strip)
import Util
import Data.List (nub)

solveDay10 input = do
    putStrLn "Day 10 - Hoof It:"
    let array = graphArray input
    let startPoints = startingPoints array
    print . part1 $ array
    print . part2 $ array

part1 array = sum . map (length . nub . map last . searchEndpoints array) . startingPoints $ array
part2 array = sum . map (length . nub . searchEndpoints array) . startingPoints $ array

graphArray input = array ((0, 0), (subtract 1 . length . head $ grid, length grid - 1)) (concat grid)
  where
    grid = xyEnumerate . map (map digitToInt) . lines . strip $ input

searchEndpoints arr start
    | arr ! start == 9 = [[]]
    | otherwise = concatMap processPathWith (neighbors arr start)
    where
        processPathWith c = map (c:) (searchEndpoints arr c)

startingPoints = map fst . filter ((== 0) . snd) . assocs

neighborCoords (x, y) = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

neighbors arr coords = filter ((== arr ! coords + 1) . (arr !)) . filter inBounds . neighborCoords $ coords
  where
    inBounds = inRange (bounds arr)
