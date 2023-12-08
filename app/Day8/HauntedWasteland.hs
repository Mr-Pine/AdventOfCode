module Day8.HauntedWasteland (solveDay8) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.Megaparsec (count, many, sepEndBy, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, space, string)
import Util (Parser, example, input, parseOrError)
import Prelude hiding (Left, Right, traverse)

solveDay8 = do
  putStrLn "Day 8 - Haunted Wasteland"
  input <- input 8
  (directions, nodeRepresentations) <- parseOrError parser input
  let graph = buildGraph "AAA" nodeRepresentations
  print "hi"
  --print $ part1 directions graph

part1 directions = takeWhile (/="ZZZ") . take 10 . map identifier . traverse (cycle directions)

data Direction = Left | Right deriving (Show, Eq)

data Node = Leaf String | Inner String Node Node deriving (Show, Eq)
identifier (Leaf n) = n
identifier (Inner n _ _) = n

data NodeRepresentation = NodeRepresentation String String String

directionsParser :: Parser [Direction]
directionsParser = many (Left <$ char 'L' <|> Right <$ char 'R')

type NodeMap = Map.Map String Node

parser :: Parser ([Direction], [NodeRepresentation])
parser = (,) <$> directionsParser <* space <*> graphRepresentationParser

graphRepresentationParser :: Parser [NodeRepresentation]
graphRepresentationParser = nodeRepresentationParser `sepEndBy` space
  where
    nodeRepresentationParser = NodeRepresentation <$> identifier <* string " = (" <*> identifier <* string ", " <*> identifier <* char ')'
    identifier = count 3 alphaNumChar :: Parser String

buildGraph :: String -> [NodeRepresentation] -> Map.Map String NodeRepresentation
buildGraph root nodeList = nodes--getNode root
  where
    nodes = Map.fromList $ map (\node@(NodeRepresentation n _ _) -> (n, node)) nodeList
    constructNode (NodeRepresentation n l r)
      | n == l && l == r = Leaf n
      | otherwise = Inner n (getNode l) (getNode r)
    getNode = constructNode . fromJust . (`Map.lookup` nodes)

traverse :: [Direction] -> Node -> [Node]
traverse = traverseGraph []
  where
    traverseGraph path _ leaf@(Leaf _) = path ++ [leaf]
    traverseGraph path (Left:xs) node@(Inner _ l r) = traverseGraph (path ++ [node]) xs l
    traverseGraph path (Right:xs) node@(Inner _ l r) = traverseGraph (path ++ [node]) xs r
