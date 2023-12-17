module Day17.ClumsyCrucible (solveDay17) where

import Algorithm.Search (dijkstra, aStar)
import Control.Applicative (ZipList (ZipList, getZipList))
import Data.Array (Ix (inRange), bounds, listArray, (!), ixmap, indices, Array)
import Data.Char (digitToInt, intToDigit)
import Data.List (transpose, groupBy, find, intercalate)
import Util (example, debugMessagePlain, input)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe, listToMaybe, fromJust)
import Control.Arrow (Arrow(first))

solveDay17 = do
  putStrLn "Day 17 - Clumsy Crucible:"
  input <- input 17
  let graph = toArray . parseString $ input
  print . part1 $ graph
  print . part2 $ graph
  -- putStrLn . visualize graph . part2 $ graph

solve reachable isEnd graph = fst . fromJust $ findRoute reachable isEnd graph (0,0)
part1 graph = solve reachableNormal isEnd graph
    where
        isEnd (Node coords _ _) = coords == (snd . bounds $ graph)
part2 graph = solve reachableUltra isEnd graph
    where
        isEnd (Node coords _ remaining) = coords == (snd . bounds $ graph) && remaining <= 6

data Direction = L | R | U | D | None deriving (Show, Eq, Ord)

directions = [L, R, U, D]


visualize graph (_, path) = intercalate "\n" $ map (map representation) graphIndices
    where
        graphIndices = transpose $ groupBy (\a b -> fst a == fst b) (indices graph)
        representation c = maybe (intToDigit (graph!c)) (directionIndicator . lastDir) (find (\n -> c==coords n && lastDir n /= None) path)
        directionIndicator L = '<'
        directionIndicator R = '>'
        directionIndicator U = '^'
        directionIndicator D = 'v'

move L (x, y) n = Node (x - 1, y) L (n-1)
move R (x, y) n = Node (x + 1, y) R (n-1)
move U (x, y) n = Node (x, y - 1) U (n-1)
move D (x, y) n = Node (x, y + 1) D (n-1)

back L = R
back R = L
back U = D
back D = U
back None = None

data Node = Node
  { coords :: (Int, Int),
    lastDir :: Direction,
    remaining :: Int
  }
  deriving (Show, Eq, Ord)

reachableNormal node@(Node coords l n) = mapMaybe steps directions
  where
    steps direction | back l == direction = Nothing
    steps direction | l == direction = if n > 0 then Just $ move direction coords n else Nothing
    steps direction = Just $ move direction coords 3

reachableUltra node@(Node coords l n) = mapMaybe steps directions
  where
    steps direction | back l == direction = Nothing
    steps direction | l == direction = if n > 0 then Just $ move direction coords n else Nothing
    steps direction = if n > 6 then Nothing else Just $ move direction coords 10

parseString = map (map digitToInt) . filter (not . null) . lines

toArray rows = listArray ((0, 0), (length (head rows) - 1, length rows - 1)) (concat . transpose $ rows)

findRoute reachable isEnd graph startC = aStar neighbors weight heuristic isEnd start
  where
    graphEnd = snd . bounds $ graph
    heuristic (Node coords _ _) = (\(x,y) (a,b) -> (a-x) + (b-y)) coords graphEnd
    neighbors = filter inBounds . reachable
    inBounds (Node coords _ _) = inRange (bounds graph) coords
    weight _ (Node coords _ _) = graph ! coords
    start = Node startC None 3