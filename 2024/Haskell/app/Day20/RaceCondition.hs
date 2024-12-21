{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Day20.RaceCondition (solveDay20) where

import Data.Array (inRange)
import Data.Hashable (Hashable)
import Data.List (find)
import Data.Map (assocs)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set, fromList, member)
import Data.Tuple.HT (mapFst, mapSnd)
import GHC.Generics (Generic)
import Util

solveDay20 input _ = do
    putStrLn "Day 20 - Race Condition:"
    --let pathBlocks = maze input
    --let endPos = end input
    --let startPos = start input
    --let avoid = [((6, 7), (5, 7))]
    --let (shortestPathLen, pred) = dijkstra (cheatyNeighbours avoid pathBlocks (bounds input)) (startPos, Unused) (endPos, Used)
    --print shortestPathLen
    --print . extractCheats (findAllOnShortestPath (endPos, Used) pred) $ pred
    print . part1 $ input

maze = fromList . map fst . concatMap (filter ((/= '#') . snd)) . xyEnumerate . lines
bounds = (\ls -> ((0, 0), (length (head ls) - 1, length ls - 1))) . lines
start = fst . fromJust . find ((== 'S') . snd) . concat . xyEnumerate . lines
end = fst . fromJust . find ((== 'E') . snd) . concat . xyEnumerate . lines

part1 input = length . filter ((>=100) . fst) . map (mapFst (baseline -)) $ findAllCheats baseline dijkstraAvoiding shortestPathFinder []
    where
        free = maze input
        startPos = start input
        endPos = end input
        mazeBounds = bounds input

        baseline = fst $ dijkstra (defaultNeigbours free) startPos endPos

        dijkstraAvoiding avoid = dijkstra (cheatyNeighbours avoid free mazeBounds) (startPos, Unused) (endPos, Used)
        shortestPathFinder = findAllOnShortestPath (endPos, Used)

defaultNeigbours :: Set Position -> Position -> [(Int, Position)]
defaultNeigbours available = map (1,) . filter (`member` available) . flip map directions . flip moveInDirection

data CheatStatus = Unused | Active | Used deriving (Enum, Generic, Eq, Ord, Show)
instance Hashable CheatStatus

type CheatablePos = (Position, CheatStatus)

cheatyNeighbours _ noWalls bounds (pos, Unused) = (0, (pos, Active)) : normalNeighbours pos ++ cheatingNeighbours pos
  where
    normalNeighbours = map (mapSnd (,Unused)) . defaultNeigbours noWalls
    cheatingNeighbours = map (mapSnd (,Active) . (1,)) . filter (inRange bounds) . flip map directions . flip moveInDirection
cheatyNeighbours avoid noWalls bounds (pos, Active)
    | pos `member` noWalls = (0, (pos, Used)) : (map (mapSnd (,Used)) . filter (not . (`elem` avoidingExits) . snd) . defaultNeigbours noWalls $ pos)
    | otherwise = map (mapSnd (,Used)) . filter (not . (`elem` avoidingExits) . snd) . defaultNeigbours noWalls $ pos
  where
    avoidingExits = map snd . filter ((== pos) . fst) $ avoid
cheatyNeighbours _ noWalls bounds (pos, _) = map (mapSnd (,Used)) . defaultNeigbours noWalls $ pos

extractCheats onShortestPath = map unpack . concatMap (\(a, bs) -> map (,a) bs) . assocs . exits
  where
    unpack = mapFst fst . mapSnd fst
    exits = Map.filter (not . null) . Map.map (filter (`member` onShortestPath) . filter ((== Active) . snd)) . Map.filterWithKey (const . (`member` onShortestPath))

findAllCheats baseline dijkstraAvoiding shortestPathFinder cheats
    | pathLen == baseline = cheats
    | otherwise = findAllCheats baseline dijkstraAvoiding shortestPathFinder (newCheats ++ cheats)
  where
    (pathLen, predecessors) = dijkstraAvoiding (map snd cheats)
    newCheats = map (pathLen,) $ extractCheats (shortestPathFinder predecessors) predecessors

type Position = (Int, Int)
type Cheat = (Position, Position)
