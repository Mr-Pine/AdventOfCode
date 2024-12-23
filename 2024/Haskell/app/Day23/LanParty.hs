module Day23.LanParty (solveDay23) where

import Data.Array (Array, listArray, (!), indices, assocs)
import Data.List (groupBy, nub, sort, intercalate)
import Data.Map (fromList, size, Map, elems, elemAt)
import Data.Set (Set, unions, intersection, member, findMin)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Tuple (swap)
import Util

solveDay23 input _ = do
    putStrLn "Day 23 - LAN Party:"
    let graph = parseGraph input
    print . part1 $ graph
    print $ part2 graph

part1 = Set.size . chiefHistorian3Cliques
part2 graph = intercalate "," . sort . Set.elems . Set.map (indexToName graph) . expandToLargestClique graph . all3Cliques $ graph

type Graph = (Map String Int, Array Int (Set Int))

indexToName graph = fst . (`elemAt` fst graph)

parseGraph pairs = (nodes pairs, edges pairs)
  where
    stringEdges = map (\e -> (take 2 e, drop 3 e)) . lines
    nodes = fromList . flip zip [0 ..] . sort . nub . uncurry (++) . unzip . stringEdges
    edges pairs = listArray (0, size (nodes pairs) - 1) . map (Set.fromList . map snd) . groupBy (\a b -> fst a == fst b) . sort . map (tmap (nodes pairs Map.!)) . concatMap (\x -> [x, swap x]) . stringEdges $ pairs

chiefHistorian3Cliques graph = possibleCliques . all3Cliques $ graph
    where
        possibleCliques = Set.filter (any (`member` possibleNodes))
        possibleNodes = Set.fromList . elems . Map.filterWithKey x . fst $ graph
        x :: String -> Int -> Bool
        x = const . (== 't') . head

all3Cliques graph = unions . map (find3Cliques graph) $ (indices . snd $ graph)

find3Cliques :: (Map String Int, Array Int (Set Int)) -> Int -> Set (Set Int)
find3Cliques graph@(nodes,edges) vertex = unions . Set.map cliquesWith $ cliqueMembers
    where
        neighbours = edges ! vertex
        neighboursNeighbours = unions . Set.map (edges !) $ neighbours

        cliqueMembers = neighboursNeighbours `intersection` neighbours

        cliquesWith x = Set.map (Set.fromList . (:[vertex,x])) . (neighbours `intersection`) . (edges !) $ x

expandClique :: Graph -> Set Int -> Set (Set Int)
expandClique graph@(nodes,edges) clique = Set.map (`Set.insert` clique) (cliqueExpanders clique)
    where
        cliqueNeighbours = Set.map (edges !)
        cliqueExpanders = foldr1 intersection . cliqueNeighbours

expandToLargestClique graph cliques
    | Set.size cliques <= 1 = findMin cliques
    | otherwise = expandToLargestClique graph expandedCliques
    where
        flatMap = (unions .) . Set.map
        expandedCliques = flatMap (expandClique graph) cliques
