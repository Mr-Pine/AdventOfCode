module Day3.GearRatios where
import Util (example, Parser, input, parseOrError)
import Text.Megaparsec.Char (string, char, space, newline, digitChar)
import Text.Megaparsec (sepBy1, (<|>), endBy1, many, oneOf)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Text (pack)
import Data.Either (isLeft, lefts, rights)
import Data.List (transpose, intercalate)
import Debug.Trace (trace)

solveDay3 = do
    putStrLn "Day 3 - Gear Ratios:"
    input <- input 3
    schematic <- parseOrError parseSchematic input
    print $ (sum . extractNumbersFromSchematic) (expand schematic)

data SchematicEntry = SchematicEntry {
    part :: PartType,
    hit:: Bool
} deriving Show

data PartType = Empty | Char Char | Symbol deriving Show
type Schematic = [[SchematicEntry]]

parseSchematic :: Parser Schematic
parseSchematic = parseSchematicLine `sepBy1` char '\n'

parseSchematicLine :: Parser [SchematicEntry]
parseSchematicLine = many parsePart
    where
        parsePart = parseEmpty <|> parseDigit <|> parseSymbol
        parseEmpty = (`SchematicEntry` False) <$> (Empty <$ char '.')
        parseDigit = (`SchematicEntry` False) . Char <$> digitChar
        parseSymbol = (`SchematicEntry` True) <$> (Symbol <$ oneOf "#$%&*+-/=@")

expandHorizontally :: [SchematicEntry] -> [SchematicEntry]
expandHorizontally = transformWindows . windowed

transformWindows :: [[Maybe SchematicEntry]] -> [SchematicEntry]
transformWindows = map transformWindow

transformWindow :: [Maybe SchematicEntry] -> SchematicEntry
transformWindow [Nothing, x, y] = transformWindow [Just (SchematicEntry Empty False), x, y]
transformWindow [Just SchematicEntry{hit = hitX}, Just SchematicEntry{part = value, hit = hitY}, Just SchematicEntry{hit = hitZ}] = SchematicEntry value (hitX || hitY || hitZ)
transformWindow [x, y, Nothing] = transformWindow [x, y, Just (SchematicEntry Empty False)]

data WindowedState = LeftEdge | Middle | RightEdge deriving (Eq, Show)

windowed :: [a] -> [[Maybe a]]
windowed = myWindows LeftEdge

myWindows :: WindowedState -> [a] -> [[Maybe a]]
myWindows LeftEdge l = (Nothing : map Just (take 2 l)) : myWindows Middle l
myWindows Middle l@(x:xs) = map Just (take 3 l) : myWindows (if length xs == 2 then RightEdge else Middle) xs
myWindows RightEdge l = [map Just (take 2 l) ++ [Nothing]]


expand = transpose . map expandHorizontally . transpose . map expandHorizontally

extractNumbersFromSchematic :: Schematic -> [Int]
extractNumbersFromSchematic = concatMap extractNumbers

extractNumbers :: [SchematicEntry] -> [Int]
extractNumbers = extractHitNumbers False []

extractHitNumbers :: Bool -> String -> [SchematicEntry] -> [Int] -- Current string hit -> NumberBuffer -> Remaining String -> Extracted Numbers
extractHitNumbers _ [] (SchematicEntry{part = Char c, hit = hit}:sx) = extractHitNumbers hit [c] sx
extractHitNumbers _ [] (s:sx) = extractHitNumbers False [] sx
extractHitNumbers alreadyHit b (SchematicEntry{part = Char c, hit = hit}:sx) = extractHitNumbers (alreadyHit || hit) (b ++ [c]) sx
extractHitNumbers hit b (s:sx)
    | hit = read b : extractHitNumbers False [] sx
    | not hit = extractHitNumbers False [] sx
extractHitNumbers _ [] [] = []
extractHitNumbers h b []
    | h = [read b]
    | not h = []