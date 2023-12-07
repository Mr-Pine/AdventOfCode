module Day7.CamelCards (solveDay7) where

import Util (example, Parser, parseOrError, Prettify (prettify), input)
import Text.Megaparsec.Char (space, char, digitChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec (sepEndBy, count, (<|>), ErrorItem (Label))
import Data.List (singleton, group, partition, sort, sortBy, find, maximumBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Function (on)

solveDay7 = do
  input <- input 7
  putStrLn "Day 7 - Camel Cards:"
  entries1 <- parseOrError (entriesParser classifyPart1) input
  entries2 <- parseOrError (entriesParser classifyPart2) input
  print $ solve part1LabelOrder entries1
  print $ solve part2LabelOrder entries2

solve labelOrder = sum . zipWith (*) [1..] . map bid . sortBy entryOrder
  where
    entryOrder :: Entry -> Entry -> Ordering
    entryOrder = handComparison `on` hand

    handComparison = compareHands (compareBy labelOrder)

    compareHands comparing a@Hand{handType = handType1, labels = label1} b@Hand{handType = handType2, labels = label2}
      | handType1 == handType2 = comparing label1 label2
      | otherwise = compare handType1 handType2

    compareBy ordering xs ys = fromMaybe EQ . find (/=EQ) $ zipWith ordering xs ys

type Classifier = [Label] -> Type
classifyPart1 :: Classifier
classifyPart1 = classifyGroups . group . sort
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
classifyPart2 labels = let
    mostCommon = maybe J (fromMaybe J . listToMaybe . maximumBy (compare `on` length)) . maybeNotEmpty . group . sort . filter (/=J) $ labels
  in classifyPart1 . map (\l -> if l == J then mostCommon else l) $ labels
  where
    maybeNotEmpty [] = Nothing
    maybeNotEmpty xs = Just xs


type LabelOrder = Label -> Label -> Ordering
part1LabelOrder :: LabelOrder
part1LabelOrder = compare
part2LabelOrder :: LabelOrder
part2LabelOrder J J = EQ
part2LabelOrder J _ = LT
part2LabelOrder _ J = GT
part2LabelOrder x y = compare x y

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