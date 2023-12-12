module Day10.PipeMaze (solveDay10) where

import Data.Array (Array, array, assocs, elems, Ix (inRange), bounds, (!))
import Data.List (group, groupBy, intercalate)
import GHC.Exts (groupWith)
import Util (debugMessagePlain, example, input)

solveDay10 = do
  putStrLn "Day 10 - Pipe Maze:"
  input <- input 10
  let array = createArray . lines $ input
  putStrLn . draw $ array
  print . part1 $ array

part1 = (`div` 2) . length . walkPath

createArray :: [String] -> Array (Int, Int) Pipe
createArray lines = array (lowerBound, upperBound) arrayValues
  where
    lowerBound = (0, 0)
    upperBound = (length lines - 1, length (head lines) - 1)

    arrayValues = concat enumeratedRows
    enumeratedRows = zipWith enumerateRow [0 ..] lines
    enumerateRow y = zipWith (\x value -> ((x, y), parsePipe value)) [0 ..]

draw :: Array (Int, Int) Pipe -> String
draw = intercalate "\n" . map (concatMap (show . snd)) . groupWith (snd . fst) . assocs

data Pipe = None | Vertical | Horizontal | BottomRight | LeftBottom | TopLeft | RightTop | Start deriving (Eq)

parsePipe '.' = None
parsePipe '|' = Vertical
parsePipe '-' = Horizontal
parsePipe 'F' = BottomRight
parsePipe '7' = LeftBottom
parsePipe 'J' = TopLeft
parsePipe 'L' = RightTop
parsePipe 'S' = Start

instance Show Pipe where
  show None = "."
  show Vertical = "┃"
  show Horizontal = "━"
  show BottomRight = "┏"
  show LeftBottom = "┓"
  show TopLeft = "┛"
  show RightTop = "┗"
  show Start = "\ESC[1;32m╋\ESC[0m"

data Direction = Up | Down | East | West deriving (Show, Eq)

availableDirections None = []
availableDirections Vertical = [Up, Down]
availableDirections Horizontal = [East, West]
availableDirections BottomRight = [Down, East]
availableDirections LeftBottom = [Down, West]
availableDirections TopLeft = [Up, West]
availableDirections RightTop = [Up, East]
availableDirections Start = [Up, Down, East, West]

opposite Up = Down
opposite Down = Up
opposite East = West
opposite West = East

walkPath :: Array (Int, Int) Pipe -> [((Int, Int), Pipe)]
walkPath array = walkNext start start
    where
        start = head . filter ((==Start) . snd) . assocs $ array

        --walkNext :: previous: ((Int, Int), Pipe) -> node: ((Int, Int), Pipe) -> pathToStart:[((Int, Int), Pipe)]
        walkNext p n | p /= start && n == start = []
        walkNext p n@(nc, np) = n : walkNext n (head . filter (/=p) . map getWithCoords . filter (inRange . bounds $ array) . map (move nc) . possibleDirections nc . availableDirections $ np)

        getWithCoords coords = (coords, array!coords)
        possibleDirections currentCoords = filter isPossible
            where
                isPossible direction = elem (opposite direction) . availableDirections . (array!) . move currentCoords $ direction

        move (x, y) Up = (x, y-1)
        move (x, y) Down = (x,y+1)
        move (x, y) East = (x+1, y)
        move (x, y) West = (x-1, y)