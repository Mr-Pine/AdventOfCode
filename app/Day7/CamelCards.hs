{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Day7.CamelCards (solveDay7) where

import Util (example, Parser, parseOrError, Prettify (prettify), input)
import Text.Megaparsec.Char (space, char, digitChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec (sepEndBy, count, (<|>), ErrorItem (Label))
import Data.List (sortOn, sort, group, maximumBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Function (on)
import Data.Coerce (coerce)
import qualified Data.Ord
import Data.Ord (comparing)
import Data.Char (digitToInt)

solveDay7 = do
  input <- input 7
  putStrLn "Day 7 - Camel Cards:"
  entries1 <- parseOrError (entriesParser LabelPart1) input
  entries2 <- parseOrError (entriesParser LabelPart2) input
  print $ solve entries1
  print $ solve entries2

solve :: Label a => [Entry a] -> Int
solve = sum . zipWith (*) [1..] . map bid . sortOn hand

data Entry a = Entry
  { hand :: Hand a,
    bid :: Int
  } deriving (Show, Eq)

newtype Label a => Hand a = Hand {labels :: [a]} deriving (Show, Eq)

instance (Label a) => Ord (Hand a) where
  compare a b = compare (classify (labels a), labels a) (classify (labels b), labels b)

class (Show a, Eq a, Ord a) => Label a where
  classify :: [a] -> Type

data LabelBase = Value Int | T | J | Q | K | A deriving (Show, Eq, Ord)
newtype LabelPart1 = LabelPart1 LabelBase deriving (Show, Eq, Prettify, Ord)
newtype LabelPart2 = LabelPart2 LabelBase deriving (Show, Eq, Prettify)

instance Label LabelPart1 where
  classify = classifyLengths . sortOn Data.Ord.Down . map length . group . sort
    where
      classifyLengths [5] = FiveOfAKind
      classifyLengths [4,_] = FourOfAKind
      classifyLengths [3,2] = FullHouse
      classifyLengths [3,_,_] = ThreeOfAKind
      classifyLengths [2,2,_] = TwoPair
      classifyLengths (2:_) = OnePair
      classifyLengths _ = HighCard

instance Label LabelPart2 where
  classify labels = classify . map (replace . toPart1) $ labels
    where
      mostCommon = maybe (LabelPart2 J) (fromMaybe (LabelPart2 J) . listToMaybe . maximumBy (compare `on` length)) . maybeNotEmpty . group . sort . filter (/=LabelPart2 J) $ labels

      replace :: LabelPart1 -> LabelPart1
      replace (LabelPart1 J) = toPart1 mostCommon
      replace x = x

      toPart1 :: LabelPart2 -> LabelPart1
      toPart1 l = LabelPart1 (coerce l :: LabelBase)

      maybeNotEmpty [] = Nothing
      maybeNotEmpty xs = Just xs

instance Ord LabelPart2 where
  compare (LabelPart2 J) (LabelPart2 J) = EQ
  compare (LabelPart2 J) _ = LT
  compare _ (LabelPart2 J) = GT
  compare x y = compare (coerce x :: LabelBase) (coerce y :: LabelBase)

instance Prettify LabelBase where
    prettify = show

data Type = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq,Ord)

type LabelConstructor a = (LabelBase -> a)
entriesParser :: Label a => LabelConstructor a -> Parser [Entry a]
entriesParser constructor = entryParser constructor `sepEndBy` space

entryParser :: Label a => LabelConstructor a -> Parser (Entry a)
entryParser constructor = Entry <$> handParser constructor <* space <*> L.decimal

handParser :: Label a => LabelConstructor a -> Parser (Hand a)
handParser construct = Hand <$> count 5 (construct <$> labelParser)
    where
        labelParser = parseA <|> parseK <|> parseQ <|> parseJ <|> parseT <|> parseT <|> (parseValueLabel <$> digitChar) :: Parser LabelBase
        parseA = A <$ char 'A' :: Parser LabelBase
        parseK = K <$ char 'K' :: Parser LabelBase
        parseQ = Q <$ char 'Q' :: Parser LabelBase
        parseJ = J <$ char 'J' :: Parser LabelBase
        parseT = T <$ char 'T' :: Parser LabelBase
        parseValueLabel = Value . digitToInt