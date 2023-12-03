module Day3.GearRatios where

import Data.Either (isLeft, lefts, rights)
import Data.List (intercalate, nub, transpose)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Debug.Trace (trace)
import System.Posix.Internals (fileType)
import Text.Megaparsec (endBy1, many, oneOf, sepBy1, (<|>))
import Text.Megaparsec.Char (char, digitChar, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Util (Parser, example, input, parseOrError)

solveDay3 = do
  putStrLn "Day 3 - Gear Ratios:"
  input <- input 3
  schematic <- parseOrError parseSchematic input
  print $ part1 schematic
  print $ part2 schematic

part1 schematic = (sum . concatMap (extractNumbers . emplaceNumbers)) (expand schematic)

part2 schematic = (sum . map gearRatio . filter ((2 ==) . length) . concat . condenseSquares processPart2 . map emplaceNumbers) (expand schematic)

gearRatio :: [SchematicPart] -> Int
gearRatio = product . map extractNumber

extractNumber (Number i@CountedInt{}) = value i
extractNumber _ = 0

extractNumbers :: [SchematicPart] -> [Int]
extractNumbers = map extractNumber . nub


data CountedInt = CountedInt
  { index :: Int,
    value :: Int
  }
  deriving (Show, Eq)

data SchematicPart = Empty | Gear | OtherSymbol | Digit Char | Number CountedInt deriving (Eq)

instance Show SchematicPart where
  show Empty = "_"
  show Gear = "*"
  show (Number CountedInt {value = x}) = show x

type GearSchematic = [[SchematicPart]]

data SchematicEntry = SchematicEntry
  { part :: SchematicPart,
    hit :: Bool
  }
  deriving (Show)

type Schematic = [[SchematicEntry]]

parseSchematic :: Parser Schematic
parseSchematic = parseSchematicLine `sepBy1` char '\n'

parseSchematicLine :: Parser [SchematicEntry]
parseSchematicLine = many parseEntry
  where
    parseEntry = parseEmpty <|> parseDigit <|> parseSymbol
    parseEmpty = (`SchematicEntry` False) <$> (Empty <$ char '.')
    parseDigit = (`SchematicEntry` False) . Digit <$> digitChar
    parseSymbol = (`SchematicEntry` True) <$> ((OtherSymbol <$ oneOf "#$%&+-/=@") <|> (Gear <$ char '*'))


expandHorizontally :: [SchematicEntry] -> [SchematicEntry]
expandHorizontally = transformWindows . windowed

expand = transpose . map expandHorizontally . transpose . map expandHorizontally


data WindowedState = LeftEdge | Middle | RightEdge deriving (Eq, Show)

windowed :: (Show a) => [a] -> [[Maybe a]]
windowed = myWindows LeftEdge

myWindows :: (Show a) => WindowedState -> [a] -> [[Maybe a]]
myWindows LeftEdge l = (Nothing : map Just (take 2 l)) : myWindows Middle l
myWindows Middle l@(x : xs) = map Just (take 3 l) : myWindows (if length xs <= 2 then RightEdge else Middle) xs
myWindows RightEdge l = [map Just (take 2 l) ++ [Nothing]]
myWindows s l = []


transformWindows :: [[Maybe SchematicEntry]] -> [SchematicEntry]
transformWindows = map transformWindow

transformWindow :: [Maybe SchematicEntry] -> SchematicEntry
transformWindow [Nothing, x, y] = transformWindow [Just (SchematicEntry Empty False), x, y]
transformWindow [Just SchematicEntry {hit = hitX}, Just SchematicEntry {part = value, hit = hitY}, Just SchematicEntry {hit = hitZ}] = SchematicEntry value (hitX || hitY || hitZ)
transformWindow [x, y, Nothing] = transformWindow [x, y, Just (SchematicEntry Empty False)]


emplaceNumbers = emplaceHitNumbers False [] 0

emplaceHitNumbers :: Bool -> String -> Int -> [SchematicEntry] -> [SchematicPart] -- Current string hit -> NumberBuffer -> Index of number in line -> Remaining String -> Extracted Numbers
emplaceHitNumbers _             []  i (SchematicEntry {part = Digit c, hit = hit}   : sx) =                                                                       emplaceHitNumbers hit                 [c]         i       sx
emplaceHitNumbers _             []  i (SchematicEntry {part = Gear}                 : sx) = Gear                                                                : emplaceHitNumbers False               []          i       sx
emplaceHitNumbers _             []  i (s                                            : sx) = Empty                                                               : emplaceHitNumbers False               []          i       sx
emplaceHitNumbers alreadyHit    b   i (SchematicEntry {part = Digit c, hit = hit}   : sx) =                                                                       emplaceHitNumbers (alreadyHit || hit) (b ++ [c])  i       sx
emplaceHitNumbers True          b   i (SchematicEntry {part = Gear}                 : sx) = replicate (length b) (Number (CountedInt i (read b)))   ++ Gear     : emplaceHitNumbers False               []          (i + 1) sx
emplaceHitNumbers True          b   i (s                                            : sx) = replicate (length b) (Number (CountedInt i (read b)))   ++ Empty    : emplaceHitNumbers False               []          (i + 1) sx
emplaceHitNumbers False         b   i (SchematicEntry {part = Gear}                 : sx) = replicate (length b) Empty                              ++ Gear     : emplaceHitNumbers False               []          i       sx
emplaceHitNumbers False         b   i (s                                            : sx) = replicate (length b + 1) Empty                          ++            emplaceHitNumbers False               []          i       sx
  where
    isGear SchematicEntry {part = Gear} = True
    isGear _ = False
emplaceHitNumbers _ [] i [] = []
emplaceHitNumbers h b i []
  | h = replicate (length b) (Number (CountedInt i (read b)))
  | not h = []


condenseSquares :: ([Maybe [Maybe SchematicPart]] -> [SchematicPart]) -> [[SchematicPart]] -> [[[SchematicPart]]]
condenseSquares processor = map (map processor) . transpose . condense . transpose . condense

condense :: (Show a) => [[a]] -> [[[Maybe a]]]
condense = map windowed


reduce :: [[[SchematicPart]]] -> [[[SchematicPart]]]
reduce = map (map (nub . removeNos))
  where
    removeNos (Empty : xs) = removeNos xs
    removeNos (x : xs) = x : removeNos xs
    removeNos [] = []


processPart2 :: [Maybe [Maybe SchematicPart]] -> [SchematicPart]
processPart2 l@[_, Just [_, Just Gear, _], _] = concatNumbers l
processPart2 _ = []

processPart1 :: [Maybe [Maybe SchematicPart]] -> [SchematicPart]
processPart1 l@[_, Just [_, Just s, _], _] | s == Gear || s == OtherSymbol = concatNumbers l
processPart1 _ = []

concatNumbers [x, y, z]= (extract . filter numberFilter) (maybe [] nub x ++ maybe [] nub y ++ maybe [] nub z)
  where
    numberFilter (Just (Number _)) = True
    numberFilter _ = False

    extract ((Just x) : xs) = x : extract xs
    extract (Nothing : xs) = extract xs
    extract [] = []