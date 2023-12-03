module Day3.GearRatios where
import Util (example, Parser, input, parseOrError, debug, debugMessage)
import Text.Megaparsec.Char (string, char, space, newline, digitChar)
import Text.Megaparsec (sepBy1, (<|>), endBy1, many, oneOf)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Text (pack)
import Data.Either (isLeft, lefts, rights)
import Data.List (transpose, intercalate, nub)
import Debug.Trace (trace)
import System.Posix.Internals (fileType)
import Data.Maybe (fromMaybe)

solveDay3 = do
    putStrLn "Day 3 - Gear Ratios:"
    input <- example 3
    schematic <- parseOrError parseSchematic input
    print $ part1 schematic
    putStrLn $ (intercalate "\n" . map show) (part2 schematic)

part1 schematic = (sum . map value . extractNumbersFromSchematic) (expand schematic)
part2 schematic = condenseSquares (map emplaceNumbers (expand schematic))

data SchematicEntry = SchematicEntry {
    part :: PartType,
    hit :: Bool
} deriving Show

data SymbolPart = Other | GearSymbol deriving Show
data PartType = Empty | Char Char | Symbol SymbolPart deriving Show
type Schematic = [[SchematicEntry]]

parseSchematic :: Parser Schematic
parseSchematic = parseSchematicLine `sepBy1` char '\n'

parseSchematicLine :: Parser [SchematicEntry]
parseSchematicLine = many parsePart
    where
        parsePart = parseEmpty <|> parseDigit <|> parseSymbol
        parseEmpty = (`SchematicEntry` False) <$> (Empty <$ char '.')
        parseDigit = (`SchematicEntry` False) . Char <$> digitChar
        parseSymbol = (`SchematicEntry` True) . Symbol <$> ((Other <$ oneOf "#$%&+-/=@") <|> (GearSymbol <$ char '*'))

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

data CountedInt = CountedInt {
    index :: Int,
    value :: Int
} deriving (Show, Eq)

extractNumbersFromSchematic :: Schematic -> [CountedInt]
extractNumbersFromSchematic = concatMap extractNumbers

extractNumbers :: [SchematicEntry] -> [CountedInt]
extractNumbers = extractHitNumbers False [] 0

extractHitNumbers :: Bool -> String -> Int -> [SchematicEntry] -> [CountedInt] -- Current string hit -> NumberBuffer -> Index of number in line -> Remaining String -> Extracted Numbers
extractHitNumbers _ [] i (SchematicEntry{part = Char c, hit = hit}:sx) = extractHitNumbers hit [c] i sx
extractHitNumbers _ [] i (s:sx) = extractHitNumbers False [] i sx
extractHitNumbers alreadyHit b i (SchematicEntry{part = Char c, hit = hit}:sx) = extractHitNumbers (alreadyHit || hit) (b ++ [c]) i sx
extractHitNumbers hit b i (s:sx)
    | hit = CountedInt i (read b) : extractHitNumbers False [] (i + 1) sx
    | not hit = extractHitNumbers False [] i sx
extractHitNumbers _ [] i [] = []
extractHitNumbers h b i []
    | h = [CountedInt i (read b)]
    | not h = []

data GearEntry = NoGear | Gear | Number CountedInt deriving (Show, Eq)
type GearSchematic = [[GearEntry]]

emplaceNumbers = emplaceHitNumbers False [] 0

emplaceHitNumbers :: Bool -> String -> Int -> [SchematicEntry] -> [GearEntry] -- Current string hit -> NumberBuffer -> Index of number in line -> Remaining String -> Extracted Numbers
emplaceHitNumbers _ [] i (SchematicEntry{part = Char c, hit = hit}:sx) = emplaceHitNumbers hit [c] i sx
emplaceHitNumbers _ [] i (SchematicEntry{part = Symbol GearSymbol}:sx) = Gear : emplaceHitNumbers False [] i sx
emplaceHitNumbers _ [] i (s:sx) = emplaceHitNumbers False [] i sx
emplaceHitNumbers alreadyHit b i (SchematicEntry{part = Char c, hit = hit}:sx) = emplaceHitNumbers (alreadyHit || hit) (b ++ [c]) i sx
emplaceHitNumbers hit b i (s:sx)
    | hit = replicate (length b) (Number (CountedInt i (read b))) ++ emplaceHitNumbers False [] (i + 1) sx
    | not hit && isGear s = Gear : emplaceHitNumbers False [] i sx
    | not hit = emplaceHitNumbers False [] i sx
    where
        isGear SchematicEntry{part = Symbol GearSymbol} = True
        isGear _ = False
emplaceHitNumbers _ [] i [] = []
emplaceHitNumbers h b i []
    | h = replicate (length b) (Number (CountedInt i (read b)))
    | not h = []

condenseSquares :: [[GearEntry]] -> [[[GearEntry]]]
condenseSquares = reduce . condenseRows . transpose . condenseColumns

reduce :: Eq a => [[[a]]] -> [[[a]]]
reduce = map (map nub)

condenseColumns :: [[GearEntry]] -> [[[GearEntry]]]
condenseColumns = map condenseColumn

condenseColumn = map (extract . filterGears) . windowed
    where
        filterGears [Just Gear, x, y] = [Nothing, x, y]
        filterGears [x, y, Just Gear] = [x, y, Nothing]
        filterGears a = debugMessage "filter Gears: " a

extract ((Just x):xs)= x : extract xs
extract (Nothing:xs)= extract xs
extract [] = []

condenseRows :: [[[GearEntry]]] -> [[[GearEntry]]]
condenseRows = map condenseRow

condenseRow :: [[GearEntry]] -> [[GearEntry]]
condenseRow = map condenseRowWindow . windowed

condenseRowWindow :: [Maybe [GearEntry]] -> [GearEntry]
condenseRowWindow [x, y, z] = maybe [] (filter noGear) x ++ fromMaybe [] y ++ maybe [] (filter noGear) z
    where
        noGear Gear = False
        noGear _ = True

completeGear :: [GearEntry] -> Bool
completeGear = (3 ==) . length