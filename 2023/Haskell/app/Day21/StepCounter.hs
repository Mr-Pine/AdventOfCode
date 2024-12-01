{-# LANGUAGE TupleSections #-}
module Day21.StepCounter (solveDay21) where
import Data.Array (listArray, Array, Ix (inRange), bounds, (!))
import Data.List (transpose, find, nub, singleton)
import Data.Maybe (fromJust)
import Util (example, input, debugMessagePlain)
import qualified Data.Set as Set
import Data.Bifunctor (Bifunctor(bimap))

solveDay21 = do
    putStrLn "Day 21 - Step Counter:"
    input <- example 21
    let (tiles, start) = parseMap input
    print tiles
    print . day1 tiles $ start
    print . day2 tiles $ start

day1 tiles = length . steps tiles 10 .  Set.singleton
day2 tiles = length . steps2 tiles 5000 .  Set.singleton
--day2 tiles = length . cycleSteps [] tiles 5000 .  Set.singleton

data Tile = Garden | Rock deriving (Show, Eq)
data StepState = StepState

steps tileMap 0 ps = ps
steps tileMap r ps = steps tileMap (r - 1) (step tileMap ps)

steps2 tileMap 0 ps = ps
steps2 tileMap r ps = steps2 tileMap (r - 1) (step2 tileMap ps)

cycleSteps seen tileMap 0 ps = ps
cycleSteps seen tileMap r ps = if ps `elem` seen then cycleSteps [ps] tileMap stillRemaining (step tileMap ps) else cycleSteps (ps:seen) tileMap (r - 1) (step2 tileMap ps)
    where
        lastDistance = fst . fromJust . find ((==ps) . snd) . zip [1..] $ seen
        stillRemaining = r `mod` lastDistance

step tileMap = foldMap singleStep
    where
        singleStep (x,y) = Set.fromList . filter ((==Garden) . (tileMap !)) . filter (inRange (bounds tileMap)) $ [(x,y-1), (x,y+1), (x-1,y), (x+1,y)]
        (x,y) `remainder` (bx,by) = (x `mod` bx, y `mod` by)

step2 tileMap = foldMap singleStep
    where
        singleStep (x,y) = Set.fromList . filter ((==Garden) . (tileMap !) . (`remainder` (snd $ bounds tileMap)))  $ [(x,y-1), (x,y+1), (x-1,y), (x+1,y)]
        (x,y) `remainder` (bx,by) = (x `mod` (bx + 1), y `mod` (by + 1))

parseMap :: String -> (Array (Int, Int) Tile, (Int, Int))
parseMap tileMap = (array, fromJust findStart)
  where
    array = toArray . filter (not . null) . lines $ tileMap

    toTile '#' = Rock
    toTile _ = Garden

    mapLines = lines tileMap
    findStart = findWithIndex ('S' `elem`) mapLines >>= findX
    findX (line, y) = (,y) . snd <$> findWithIndex (=='S') line
    findWithIndex :: (a -> Bool) -> [a] -> Maybe (a, Int)
    findWithIndex p l = find (p . fst) (zip l [0..])
    toArray rows = listArray ((0, 0), (length (head rows) - 1, length rows - 1)) (map toTile . concat . transpose $ rows)