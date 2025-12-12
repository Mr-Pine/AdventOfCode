module Day12.ChristmasTreeFarm (solveDay12) where

import Control.Applicative ((<|>))
import Control.Exception (assert)
import Text.Megaparsec (sepEndBy1, some, try)
import Text.Megaparsec.Char (char, hspace, newline)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Util

solveDay12 input _ = do
    putStrLn "Day 12 - Christmas Tree Farm:"
    problem <- parseOrError problemParser input
    print . uncurry dirty $ problem

dirty shapes = length . filter assertBoxCountEnough . filter enoughSpace
  where
    assertBoxCountEnough r@(Region (x, y) cs) = assert (x `div` 3 * y `div` 3 >= sum cs) True
    enoughSpace (Region (x, y) cs) = x * y >= sum (zipWith (*) shapeSizes cs)
    shapeSizes = map (length . concatMap (filter id)) shapes

type Shape = [[Bool]]
data Region = Region (Int, Int) [Int] deriving (Show)

shapeParser :: Parser Shape
shapeParser = Lex.decimal *> char ':' *> newline *> some (True <$ char '#' <|> False <$ char '.') `sepEndBy1` newline

regionParser :: Parser Region
regionParser = Region <$> ((,) <$> Lex.decimal <* char 'x' <*> Lex.decimal) <* char ':' <* hspace <*> (Lex.decimal `sepEndBy1` hspace)

problemParser = (,) <$> try shapeParser `sepEndBy1` newline <*> regionParser `sepEndBy1` newline
