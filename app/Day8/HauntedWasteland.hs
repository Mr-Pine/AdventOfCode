module Day8.HauntedWasteland (solveDay8) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.Megaparsec (count, many, sepEndBy, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, space, string)
import Util (Parser, example, input, parseOrError, debugMessage, debugMessagePlain)
import Prelude hiding (traverse)

solveDay8 = do
  putStrLn "Day 8 - Haunted Wasteland"
  input <- input 8
  (directions, nodeRepresentations) <- parseOrError parser input
  let graph = nodeMap nodeRepresentations
  print $ part1 directions graph
  print $ part2 directions graph

part1 directions nodes = length . takeWhile ((/="ZZZ") . identifier) . traverse nodes (cycle directions) $ root
  where
    root = nodes Map.! "AAA"

part2 directions nodes = foldl1 lcm . map (length . takeWhile isNotEnd . traverse nodes (cycle directions)) $ roots
  where
    roots = filter isStart (Map.elems nodes)
    isStart (Node [_, _, 'A'] _ _) = True
    isStart _ = False

    isNotEnd (Node [_, _, 'Z'] _ _) = False
    isNotEnd _ = True

data Direction = L | R deriving (Show, Eq)

data Node = Node String String String deriving (Show, Eq)
identifier (Node n _ _) = n

type NodeMap = Map.Map String Node


nodeMap :: [Node] -> NodeMap
nodeMap nodeList = nodes
  where
    nodes = Map.fromList . map (\node -> (identifier node, node)) $ nodeList

traverse :: NodeMap -> [Direction] -> Node -> [Node]
traverse nodes (d:ds) node = node : traverse nodes ds (nextNode nodes d node)
  where
    nextNode nodes L (Node _ l _) = nodes Map.! l
    nextNode nodes R (Node _ _ r) = nodes Map.! r

directionsParser :: Parser [Direction]
directionsParser = many (L <$ char 'L' <|> R <$ char 'R')

parser :: Parser ([Direction], [Node])
parser = (,) <$> directionsParser <* space <*> nodesParser

nodesParser :: Parser [Node]
nodesParser = nodeRepresentationParser `sepEndBy` space
  where
    nodeRepresentationParser = Node <$> identifier <* string " = (" <*> identifier <* string ", " <*> identifier <* char ')'
    identifier = count 3 alphaNumChar :: Parser String
