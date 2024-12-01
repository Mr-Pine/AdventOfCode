module Day10.PipeMaze (solveDay10) where

import Data.Array (Array, array, assocs, elems, Ix (inRange), bounds, (!))
import Data.List (group, groupBy, intercalate)
import GHC.Exts (groupWith)
import Util (debugMessagePlain, example, input, debugPlain)

solveDay10 = do
  putStrLn "Day 10 - Pipe Maze:"
  input <- input 10
  let array = createArray . lines $ input
  let path = walkPath array
  putStrLn . draw $ array
  print . part1 $ path
  print . part2 array $ path

part2 = interiorPoints
part1 :: [a] -> Int
part1 = (`div` 2) . length

createArray :: [String] -> Array (Int, Int) Pipe
createArray lines = array (lowerBound, debugPlain upperBound) arrayValues
  where
    lowerBound = (0, 0)
    upperBound = (length (head lines) - 1, length lines - 1)

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
        walkNext p n = n : walkNext n (head . filter (/=p) . neighbors array $ n)



neighbors :: Array (Int, Int) Pipe -> ((Int, Int), Pipe) -> [((Int, Int), Pipe)]
neighbors array (coords, pipe) = map getWithCoords possibleNeighborCoords
    where
        possibleNeighborCoords = filter (inRange . bounds $ array) . map (move coords) . possibleDirections $ coords
        possibleDirections currentCoords = filter isPossible . availableDirections $ pipe
            where
                isPossible direction = maybe False (elem (opposite direction) . availableDirections . (array!)) (justIf (inRange . bounds $ array) . move currentCoords $ direction)
                justIf p a
                    | p a = Just a
                    | otherwise = Nothing

        move (x, y) Up = (x, y-1)
        move (x, y) Down = (x,y+1)
        move (x, y) East = (x+1, y)
        move (x, y) West = (x-1, y)

        getWithCoords coords = (coords, array!coords)

getArea :: Array (Int, Int) Pipe -> [((Int, Int), Pipe)] -> Int
getArea array (s@(sc, _):xs) = abs . (`div` 2) . sum . map edgeValue $ edges
    where
        corners = filter (isCorner . snd) loop
        edges = zip corners (drop 1 . cycle $ corners)
        edgeValue (((x1,y1), _), ((x2,y2), _)) = (y1+y2) * (x1-x2)

        loop = (sc, startReplacement) : xs


        replacements = [Vertical, Horizontal, BottomRight, LeftBottom, TopLeft, RightTop]
        startReplacement = head . filter validReplacement $ replacements
        validReplacement pipe = sNeighbors == replacementNeighbors || reverse sNeighbors == replacementNeighbors
            where
                sNeighbors = neighbors array s
                replacementNeighbors = neighbors array (sc, pipe)

        isCorner Horizontal = False
        isCorner Vertical = False
        isCorner _ = True

interiorPoints array loop = getArea array loop + 1 - (length loop `div` 2)