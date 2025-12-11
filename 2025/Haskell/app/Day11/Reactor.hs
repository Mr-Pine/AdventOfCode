module Day11.Reactor (solveDay11) where

import Data.Array.IArray (Array, listArray, (!))
import Text.Megaparsec (sepEndBy1, some)
import Text.Megaparsec.Char (alphaNumChar, char, hspace, newline)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Util

solveDay11 input _ = do
    putStrLn "Day 11 - Reactor:"
    nodes <- parseOrError nodesParser input
    print . part1 $ nodes
    print . part2 $ nodes

part1 nodes = pathsBetween nodes "you" "out"
part2 nodes = paths ["svr", "dac", "fft", "out"] + paths ["svr", "fft", "dac", "out"] where
    paths = product . map (uncurry (pathsBetween nodes) . (\[a,b] -> (a,b))) . windows 2

pathsBetween nodes from to = pathCount (indexOf to) ! indexOf from
  where
    indexOf "out" = length nodes
    indexOf lbl = fst . head . filter ((== lbl) . label . snd) . zip [0 ..] $ nodes

    adjacencyArray = listArray (0, length nodes - 1) (map (map indexOf . neighbors) nodes) :: Array Int [Int]

    pathCount :: Int -> Array Int Int
    pathCount to = listArray (0, length nodes) (map (countPaths to) [0 .. length nodes])
    countPaths to n
        | n == to = 1
        | n == length nodes = 0
        | otherwise = sum . map (pathCount to !) $ adjacencyArray ! n

data Node = Node {label :: String, neighbors :: [String]} deriving (Show)

nodeParser :: Parser Node
nodeParser = Node <$> some alphaNumChar <* char ':' <* hspace <*> some alphaNumChar `sepEndBy1` hspace

nodesParser :: Parser [Node]
nodesParser = nodeParser `sepEndBy1` newline
