module Day7.Laboratories (solveDay7) where

import Control.Applicative ((<|>))
import Data.Array.IArray (Array, Ix (range), elems, listArray, (!), (!?))
import Data.List (groupBy, intercalate)
import Data.Maybe (fromMaybe)
import Text.Megaparsec (sepEndBy1, some)
import Text.Megaparsec.Char (char, newline)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Util

solveDay7 input _ = do
    putStrLn "Day 7 - Laboratories:"
    diagram <- parseOrError diagramParser input
    print . splitterHits $ diagram

data DiagramEntry = Start | Empty | Splitter deriving (Show, Eq)

displayDiagramWithBeam width = chunked width . map (uncurry displayEntry)
  where
    displayEntry _ True = '|'
    displayEntry Start _ = 'S'
    displayEntry Empty _ = '.'
    displayEntry Splitter _ = '^'

-- intercalate "\n" . displayDiagramWithBeam (length (head diagram)) $ zip (concat diagram) (elems arr)
splitterHits diagram = length . filter (== (Splitter, True)) $ zip (concat diagram) (elems arr)
  where
    arr = listArray indexRange (map (uncurry hitMap) indexWithEntry)
    splitterArr = listArray indexRange (map ((== Splitter) . snd) indexWithEntry) :: Array (Int, Int) Bool
    indexRange = ((0, 0), (length diagram - 1, length (head diagram) - 1))
    indexWithEntry = zip (range indexRange) (concat diagram)
    {-# INLINE (!?!) #-}
    (!?!) :: (Ix i) => Array i Bool -> i -> Bool
    (!?!) a i = fromMaybe False (a !? i)
    hitMap _ Start = True
    hitMap (y, x) Empty = (splitterArr !?! (y, x - 1) && arr !?! (y, x - 1)) || (splitterArr !?! (y, x + 1) && arr !?! (y, x + 1)) || (not (splitterArr !?! (y - 1, x)) && arr !?! (y - 1, x))
    hitMap (y, x) Splitter = arr !?! (y - 1, x)

entryParser :: Parser DiagramEntry
entryParser = Start <$ char 'S' <|> Empty <$ char '.' <|> Splitter <$ char '^'

diagramParser :: Parser [[DiagramEntry]]
diagramParser = some entryParser `sepEndBy1` newline
