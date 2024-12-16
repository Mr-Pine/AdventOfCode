module Day16.ReindeerMaze (solveDay16) where

import Data.HashPSQ (HashPSQ, alter, minView, singleton)
import Data.Map ((!))
import Data.Set (Set, fromList, insert, member)
import Data.Tuple.HT (mapFst)
import Util
import Prelude hiding (Right)

solveDay16 input _ = do
    putStrLn "Day 16 - Reindeer Maze:"
    print . part1 $ input

part1 input = findPath maze end startPQ
  where
    startPQ = singleton (start, Right) 0 ()
    (maze, start, end) = parseMaze (lines input)

findPath :: Set (Int,Int) -> (Int,Int) -> HashPSQ ((Int,Int),Direction) Int () -> Int
findPath maze target priorityQueue =
    if pos == target then distance else findPath maze target updatedNeighbors
  where
    Just (node@(pos, dir), distance, (), remaining) = minView priorityQueue
    neighbours = filter (not . (`member` maze) . fst . snd) [(1000, (pos, rotateCounterClockwise dir)), (1, (moveInDirection dir pos, dir)), (1000, (pos, rotateClockwise dir))]
    neighboursRealDistances = map (mapFst (+ distance)) neighbours
    neighboursDistanceUpdaters = map (mapFst alterDistance) neighboursRealDistances

    updatedNeighbors = foldr ((snd .) . uncurry alter) remaining neighboursDistanceUpdaters

    alterDistance new (Just (old, _)) = ((), Just (min new old, ()))
    alterDistance new Nothing = ((), Just (new, ()))

parseMaze input = (walls, start, end)
  where
    enumerated = xyEnumerate input
    start = fst . head . concatMap (filter ((== 'S') . snd)) $ enumerated
    end = fst . head . concatMap (filter ((== 'E') . snd)) $ enumerated

    walls = fromList . concatMap (map fst . filter ((== '#') . snd)) $ enumerated
