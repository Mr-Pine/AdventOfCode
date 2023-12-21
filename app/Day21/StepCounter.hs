{-# LANGUAGE TupleSections #-}
module Day21.StepCounter (solveDay21) where
import Data.Array (listArray, Array, Ix (inRange), bounds, (!))
import Data.List (transpose, find, nub)
import Data.Maybe (fromJust)
import Util (example, input)

solveDay21 = do
    putStrLn "Day 21 - Step Counter:"
    input <- input 21
    let (tiles, start) = parseMap input
    print . day1 tiles $ [start]

day1 tiles = length . steps tiles 64 

data Tile = Garden | Rock deriving (Show, Eq)

steps tileMap 0 ps = ps
steps tileMap r ps = steps tileMap (r - 1) (step tileMap ps)

step tileMap = nub . concatMap singleStep
    where
        singleStep (x,y) = filter ((==Garden) . (tileMap !)) $ filter (inRange . bounds $ tileMap) [(x,y-1), (x,y+1), (x-1,y), (x+1,y)]

parseMap :: String -> (Array (Int, Int) Tile, (Int, Int))
parseMap tileMap = (array, fromJust findStart)
  where
    array = toArray . lines $ tileMap

    toTile '#' = Rock
    toTile _ = Garden

    mapLines = lines tileMap
    findStart = findWithIndex ('S' `elem`) mapLines >>= findX
    findX (line, y) = (,y) . snd <$> findWithIndex (=='S') line
    findWithIndex :: (a -> Bool) -> [a] -> Maybe (a, Int)
    findWithIndex p l = find (p . fst) (zip l [0..])
    toArray rows = listArray ((0, 0), (length (head rows) - 1, length rows - 1)) (map toTile . concat . transpose $ rows)