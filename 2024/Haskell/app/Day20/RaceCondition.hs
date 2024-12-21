{-# LANGUAGE TupleSections #-}

module Day20.RaceCondition (solveDay20) where

import Data.Array (inRange)
import Data.Hashable (Hashable)
import Data.List (find)
import Data.Map ((!), (!?), fromList)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set, member, toList, union, empty, size)
import Util
import qualified Data.Set as Set
import Data.Tuple (swap)

solveDay20 input _ = do
    putStrLn "Day 20 - Race Condition:"
    print . part1 $ input
    print . part2 $ input

maze = Set.fromList . map fst . concatMap (filter ((/= '#') . snd)) . xyEnumerate . lines
bounds = (\ls -> ((0, 0), (length (head ls) - 1, length ls - 1))) . lines
start = fst . fromJust . find ((== 'S') . snd) . concat . xyEnumerate . lines
end = fst . fromJust . find ((== 'E') . snd) . concat . xyEnumerate . lines

part1 input = size . Set.filter ((>=100) . fst) $ findCheats input 2
part2 input = size . Set.filter ((>=100) . fst) $ findCheats input 20

findCheats input cheatLength = allCheats
    where
        defaultPath = dijkstra (defaultNeigbours (maze input)) (start input) (end input)
        path = zip [0..] . reverse . (`buildPath` end input) . snd $ defaultPath
        pathSet = Set.fromList . map snd $ path
        pathMap = fromList . map swap $ path
        mazeBounds = bounds input

        allCheats = Set.filter ((>0) . fst) . Set.foldr union empty . Set.map cheats $ pathSet

        evalCheat from to = (pathMap ! to - pathMap ! from - manhattanDistance from to, (from,to))

        cheats from = Set.map (evalCheat from) .  Set.filter ((<= cheatLength) . manhattanDistance from) $ pathSet

        manhattanDistance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

        buildPath preds pos = pos : maybe [] (buildPath preds . head) (preds !? pos)

defaultNeigbours available = map (1,) . filter (`member` available) . flip map directions . flip moveInDirection
