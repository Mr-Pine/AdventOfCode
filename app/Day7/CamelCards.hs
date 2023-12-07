{-# LANGUAGE OverloadedRecordDot #-}
module Day7.CamelCards (solveDay7) where

import Util (example, Parser, parseOrError, debugMessage, Prettify (prettify), debug, debugMessagePlain, input)
import Text.Megaparsec.Char (space, char, digitChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec (sepEndBy, count, (<|>))
import Data.List (singleton, group, partition, sort)

solveDay7 = do
  input <- input 7
  putStrLn "Day 7 - Camel Cards:"
  entries <- parseOrError entriesParser input
  print $ part1 entries

part1 = sum . zipWith (*) [1..] . map bid . sort

data Entry = Entry
  { hand :: Hand,
    bid :: Int
  } deriving (Show, Eq)

instance Ord Entry where
    compare a b = compare a.hand b.hand

data Hand = Hand
  { labels :: [Label],
    handType :: Type
  } deriving (Show, Eq)


data Label = Value Int | T | J | Q | K | A deriving (Show, Eq, Ord)

instance Prettify Label where
    prettify = show

data Type = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq,Ord)

instance Ord Hand where
    compare a@Hand{handType = handType1, labels = label1} b@Hand{handType = handType2, labels = label2}
        | handType1 == handType2 = compare label1 label2
        | otherwise = compare handType1 handType2

entriesParser :: Parser [Entry]
entriesParser = entryParser `sepEndBy` space

entryParser :: Parser Entry
entryParser = Entry <$> handParser <* space <*> L.decimal

handParser :: Parser Hand
handParser = getHand <$> count 5 labelParser
    where
        labelParser = parseA <|> parseK <|> parseQ <|> parseJ <|> parseT <|> parseT <|> (parseValueLabel <$> digitChar) :: Parser Label
        parseA = A <$ char 'A' :: Parser Label
        parseK = K <$ char 'K' :: Parser Label
        parseQ = Q <$ char 'Q' :: Parser Label
        parseJ = J <$ char 'J' :: Parser Label
        parseT = T <$ char 'T' :: Parser Label
        parseValueLabel = Value . read . singleton

        getHand labels = Hand labels (classify labels)
        classify = classifyGroups . groupBy2 (==)
        classifyGroups xs
            | length xs == 1 = FiveOfAKind
            | any ((==4) . length) xs = FourOfAKind
            | any ((==3) . length) xs && any ((==2) . length) xs = FullHouse
            | any ((==3) . length) xs = ThreeOfAKind
            | (length . filter ((==2) . length)) xs == 2 = TwoPair
            | any ((==2) . length) xs = OnePair
            | otherwise = HighCard

groupBy2 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy2 = go [] where
    go acc comp [] = acc
    go acc comp (h:t) =
        let (hs, nohs) = partition (comp h) t
        in go ((h:hs):acc) comp nohs