module Day7.Laboratories (solveDay7) where

import Control.Applicative ((<|>))
import Data.Array.IArray (Array, Ix (range), elems, listArray, (!), (!?))
import Data.Char (intToDigit)
import Data.List (groupBy, intercalate)
import Data.Maybe (fromMaybe)
import Text.Megaparsec (sepEndBy1, some)
import Text.Megaparsec.Char (char, newline)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Util

solveDay7 input _ = do
    putStrLn "Day 7 - Laboratories:"
    diagram <- parseOrError diagramParser input
    print . part1 $ diagram
    print . part2 $ diagram

data DiagramEntry = Start | Empty | Splitter deriving (Show, Eq)

part1 diagram = length . filter (\(e, c) -> e == Splitter && c > 0) $ zip (concat diagram) (elems (beamArray diagram))
part2 diagram = sum . take (length (head diagram)) . reverse . elems . beamArray $ diagram

displayDiagramWithBeam width = chunked width . map (uncurry displayEntry)
  where
    displayEntry Start 0 = 'S'
    displayEntry Empty 0 = '.'
    displayEntry Splitter 0 = '^'
    displayEntry _ n = intToDigit n

-- splitterHits diagram = length . filter (== (Splitter, True)) $ zip (concat diagram) (elems arr)
beamArray diagram = arr
  where
    arr = listArray indexRange (map (uncurry hitMap) indexWithEntry) :: Array (Int, Int) Int
    splitterArr = listArray indexRange (map ((== Splitter) . snd) indexWithEntry) :: Array (Int, Int) Bool
    indexRange = ((0, 0), (length diagram - 1, length (head diagram) - 1))
    indexWithEntry = zip (range indexRange) (concat diagram)
    {-# INLINE (!?!) #-}
    (!?!) a i = fromMaybe 0 (a !? i)
    hitMap _ Start = 1
    hitMap (y, x) Empty = sum . map snd . filter (fromMaybe False . fst) $ [(splitterArr !? (y, x - 1), arr !?! (y, x - 1)), (splitterArr !? (y, x + 1), arr !?! (y, x + 1)), (Just . not . fromMaybe False $ (splitterArr !? (y - 1, x)), arr !?! (y - 1, x))]
    hitMap (y, x) Splitter = arr !?! (y - 1, x)

entryParser :: Parser DiagramEntry
entryParser = Start <$ char 'S' <|> Empty <$ char '.' <|> Splitter <$ char '^'

diagramParser :: Parser [[DiagramEntry]]
diagramParser = some entryParser `sepEndBy1` newline
