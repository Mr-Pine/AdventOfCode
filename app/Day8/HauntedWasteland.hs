module Day8.HauntedWasteland (solveDay8) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.Megaparsec (count, many, sepEndBy, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, space, string)
import Util (Parser, example, input, parseOrError, debugMessage, debugMessagePlain)
import Prelude hiding (Left, Right, traverse)

solveDay8 = do
  putStrLn "Day 8 - Haunted Wasteland"
  input <- input 8
  (directions, nodeRepresentations) <- parseOrError parser input
  let graph = nodeMap nodeRepresentations
  print $ part1 directions graph

part1 :: [Direction] -> NodeMap -> Int
part1 directions nodes = length . takeWhile (/="ZZZ") . map identifier . traverse nodes (cycle directions) $ root
  where
    root = fromJust $ Map.lookup "AAA" nodes

data Direction = Left | Right deriving (Show, Eq)

data Node = Leaf String | Inner String String String deriving (Show, Eq)
identifier (Leaf n) = n
identifier (Inner n _ _) = n

directionsParser :: Parser [Direction]
directionsParser = many (Left <$ char 'L' <|> Right <$ char 'R')

type NodeMap = Map.Map String Node

parser :: Parser ([Direction], [Node])
parser = (,) <$> directionsParser <* space <*> graphRepresentationParser

graphRepresentationParser :: Parser [Node]
graphRepresentationParser = nodeRepresentationParser `sepEndBy` space
  where
    nodeRepresentationParser = buildNode <$> identifier <* string " = (" <*> identifier <* string ", " <*> identifier <* char ')'
    identifier = count 3 alphaNumChar :: Parser String
    buildNode n l r
      | n == l && l == r = Leaf n
      | otherwise = Inner n l r

nodeMap :: [Node] -> NodeMap
nodeMap nodeList = nodes--getNode root
  where
    nodes = Map.fromList . map (\node -> (identifier node, node)) $ nodeList

traverse :: NodeMap -> [Direction] -> Node -> [Node]
traverse = traverseGraph []
  where
    traverseGraph path _ _ node | identifier node == "ZZZ" = path
    traverseGraph path nodes _ leaf@(Leaf _) = leaf : path
    traverseGraph path nodes (Left:xs) node@(Inner _ l r) = traverseGraph (node : path) nodes xs (fromJust $ Map.lookup l nodes)
    traverseGraph path nodes (Right:xs) node@(Inner _ l r) = traverseGraph (node : path) nodes xs (fromJust $ Map.lookup r nodes)
