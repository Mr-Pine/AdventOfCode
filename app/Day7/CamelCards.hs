{-# LANGUAGE OverloadedRecordDot #-}
module Day7.CamelCards (solveDay7) where

import Util (example, Parser, parseOrError, debugMessage, Prettify (prettify), debug, debugMessagePlain, input)
import Text.Megaparsec.Char (space, char, digitChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec (sepEndBy, count, (<|>))
import Data.List (singleton, group, partition, sort, sortBy, find, maximumBy)
import Data.Maybe (fromMaybe)
import Data.Function (on)

solveDay7 = do
  input <- example 7
  putStrLn "Day 7 - Camel Cards:"
  entries1 <- parseOrError (entriesParser classifyPart1) input
  entries2 <- parseOrError (entriesParser classifyPart2) input
  --print $ solve part1LabelOrder entries1
  print $ solve part2LabelOrder entries2
  print $ classifyPart2 [J,J,Value 2, Value 3, Value 4]

solve :: (Label -> Label -> Ordering) -> [Entry] -> Int
solve labelOrder = sum . zipWith (*) [1..] . map bid . debugMessagePlain "Sorted" . sortBy entryOrder
  where
    entryOrder :: Entry -> Entry -> Ordering
    entryOrder a b = handComparison a.hand b.hand

    handComparison = compareHands (compareBy labelOrder)

    compareHands comparing a@Hand{handType = handType1, labels = label1} b@Hand{handType = handType2, labels = label2}
      | handType1 == handType2 = comparing label1 label2
      | otherwise = compare handType1 handType2

    compareBy ordering xs ys = fromMaybe EQ $ find (/=EQ) (zipWith ordering xs ys)

type Classifier = [Label] -> Type
classifyPart1 :: Classifier
classifyPart1 = classifyGroups . groupBy2 (==)
  where
    classifyGroups xs
      | length xs == 1 = FiveOfAKind
      | any ((==4) . length) xs = FourOfAKind
      | any ((==3) . length) xs && any ((==2) . length) xs = FullHouse
      | any ((==3) . length) xs = ThreeOfAKind
      | (length . filter ((==2) . length)) xs == 2 = TwoPair
      | any ((==2) . length) xs = OnePair
      | otherwise = HighCard


classifyPart2 :: Classifier
classifyPart2 = classifyPart1 . findBest []
  where
    jReplacements = [T, Q, K, A] ++ map Value [2..9]
    findBest xs (J:ys) = maximumBy (compare `on` classifyPart1) $ map (\replacement -> findBest xs (replacement : ys)) jReplacements
    findBest xs (y:ys) = findBest (xs ++ [y]) ys
    findBest xs [] = xs


type LabelOrder = Label -> Label -> Ordering
part1LabelOrder :: LabelOrder
part1LabelOrder = compare
part2LabelOrder :: LabelOrder
part2LabelOrder J _ = LT
part2LabelOrder _ J = GT
part2LabelOrder x y = compare x y

type LabelEquality = Label -> Label -> Bool
part1LabelEquality :: LabelEquality
part1LabelEquality = (==)
part2LabelEquality :: LabelEquality
part2LabelEquality x y = x == y || x == J || y == J

data Entry = Entry
  { hand :: Hand,
    bid :: Int
  } deriving (Show, Eq)

data Hand = Hand
  { labels :: [Label],
    handType :: Type
  } deriving (Show, Eq)


data Label = Value Int | T | J | Q | K | A deriving (Show, Eq, Ord)

instance Prettify Label where
    prettify = show

data Type = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq,Ord)

entriesParser :: Classifier -> Parser [Entry]
entriesParser classify = entryParser classify `sepEndBy` space

entryParser :: Classifier -> Parser Entry
entryParser classify = Entry <$> handParser classify <* space <*> L.decimal

handParser :: Classifier -> Parser Hand
handParser classify = getHand <$> count 5 labelParser
    where
        labelParser = parseA <|> parseK <|> parseQ <|> parseJ <|> parseT <|> parseT <|> (parseValueLabel <$> digitChar) :: Parser Label
        parseA = A <$ char 'A' :: Parser Label
        parseK = K <$ char 'K' :: Parser Label
        parseQ = Q <$ char 'Q' :: Parser Label
        parseJ = J <$ char 'J' :: Parser Label
        parseT = T <$ char 'T' :: Parser Label
        parseValueLabel = Value . read . singleton

        getHand labels = Hand labels (classify labels)

groupBy2 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy2 = go [] where
    go acc comp [] = acc
    go acc comp (h:t) =
        let (hs, nohs) = partition (comp h) t
        in go ((h:hs):acc) comp nohs