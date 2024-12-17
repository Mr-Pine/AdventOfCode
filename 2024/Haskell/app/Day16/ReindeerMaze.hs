{-# LANGUAGE TupleSections #-}

module Day16.ReindeerMaze (solveDay16) where

import Control.Applicative ((<|>))
import Data.HashPSQ (HashPSQ, alter, minView, singleton)
import qualified Data.HashPSQ as HashPSQ
import Data.Map ((!), (!?))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList, insert, member, size)
import qualified Data.Set as Set
import Data.Tuple.HT (mapFst)
import Debug.Trace (trace)
import Util
import Prelude hiding (Left, Right)
import Data.List (intercalate)

solveDay16 input _ = do
    putStrLn "Day 16 - Reindeer Maze:"
    let (maze, start, end) = parseMaze (lines input)
    let startPQ = singleton (start, Right) 0 ()
    let (distance, predecessors) = findPath maze end startPQ Set.empty Map.empty
    print distance
    let pathBlocks = part2 predecessors start end
    --putStrLn . displayMaze (length (head (lines input)), length (lines input)) maze $ pathBlocks
    print . size $ pathBlocks

displayMaze (width, height) maze path = intercalate "\n" . map (map toChar) $ positions
  where
    positions = map (map fst) . xyEnumerate . replicate height . replicate width $ ()

    toChar p
        | p `Set.member` maze = '#'
        | p `Set.member` path = 'O'
        | otherwise = '.'

type Node = ((Int, Int), Direction)

part2 predecessors start end = Set.map fst $ walkPredecessors (filter (`Map.member` predecessors) (map (end,) [Up, Down, Left, Right])) Set.empty predecessors
  where
    walkPredecessors :: [Node] -> Set Node -> Map Node [Node] -> Set Node
    walkPredecessors [] visited predecessors = visited
    walkPredecessors (p : ps) visited predecessors = walkPredecessors (maybe [] (filter (not . (`member` visited))) (predecessors !? p) ++ ps) (p `insert` visited) predecessors

findPath :: Set (Int, Int) -> (Int, Int) -> HashPSQ Node Int () -> Set Node -> Map Node [Node] -> (Int, Map Node [Node])
findPath maze target priorityQueue processed predecessors =
    if pos == target then (distance, predecessors) else findPath maze target updatedNeighbors (node `Set.insert` processed) updatedPredecessors
  where
    Just (node@(pos, dir), distance, (), remaining) = minView priorityQueue
    neighbours = filter (not . (`member` processed) . snd) . filter (not . (`member` maze) . fst . snd) $ [(1000, (pos, rotateCounterClockwise dir)), (1, (moveInDirection dir pos, dir)), (1000, (pos, rotateClockwise dir))]
    neighbourUpdaters = map (mapFst (+ distance)) neighbours

    (updatedNeighbors, updatedPredecessors) = foldr updateNode (remaining, predecessors) neighbourUpdaters

    updateNode (newDistance, newPosition) (prioQueue, predecessorMap) = (updatedPrioQueue, updatedPredecessorMap)
      where
        oldDistance = fst <$> newPosition `HashPSQ.lookup` prioQueue
        minDistance = maybe newDistance (min newDistance) oldDistance

        updatedPrioQueue = snd $ alter (const ((), Just (minDistance, ()))) newPosition prioQueue
        updatedPredecessorMap = case compare newDistance <$> oldDistance of
            Just GT -> predecessorMap
            Just EQ -> Map.alter ((node :) <$>) newPosition predecessorMap
            Just LT -> Map.insert newPosition [node] predecessorMap
            Nothing -> Map.alter ((<|> Just [node]) . (id <$>)) newPosition predecessorMap

    alterDistance new (Just (old, _)) = ((), Just (min new old, ()))
    alterDistance new Nothing = ((), Just (new, ()))

parseMaze input = (walls, start, end)
  where
    enumerated = xyEnumerate input
    start = fst . head . concatMap (filter ((== 'S') . snd)) $ enumerated
    end = fst . head . concatMap (filter ((== 'E') . snd)) $ enumerated

    walls = fromList . concatMap (map fst . filter ((== '#') . snd)) $ enumerated
