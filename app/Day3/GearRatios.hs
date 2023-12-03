module Day3.GearRatios where
import Util (example, Parser, input, parseOrError, debug, debugMessage, debugMessageWith)
import Text.Megaparsec.Char (string, char, space, newline, digitChar)
import Text.Megaparsec (sepBy1, (<|>), endBy1, many, oneOf)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Text (pack)
import Data.Either (isLeft, lefts, rights)
import Data.List (transpose, intercalate, nub)
import Debug.Trace (trace)
import System.Posix.Internals (fileType)
import Data.Maybe (fromMaybe)
import Text.Read (Lexeme(String))

solveDay3 = do
    putStrLn "Day 3 - Gear Ratios:"
    --input <- readFile ("./input/" ++ "3_kennwort" ++ ".input")
    input <- example 3
    schematic <- parseOrError parseSchematic input
    print $ part1 schematic
    putStrLn (part2Show schematic)
    print (part2 schematic)

part1 schematic = (sum . map value . extractNumbersFromSchematic) (expand schematic)
--part2 schematic = (intercalate "\n" . map beautify . condenseSquares . map emplaceNumbers) (expand schematic)
-- part2 schematic = (beautify . filter ((2 ==) . length) . concat . transpose . condenseSquares . map emplaceNumbers) (expand schematic)
part2Show schematic = (intercalate "\n" . map beautify . condenseSquares . debugMessage "emplaced: " . map emplaceNumbers) (expand schematic)
part2 schematic = (sum . map gearRatio . filter ((2 ==) . length) . concat . condenseSquares . map emplaceNumbers) (expand schematic)

gearRatio :: [GearEntry] -> Int
gearRatio = product . debug . map extractNumber
    where
        extractNumber (Number CountedInt{value = x}) = x

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

windowed :: Show a => [a] -> [[Maybe a]]
windoed [] = debugMessage "Trying to window empty" []
windowed = myWindows LeftEdge

myWindows :: Show a => WindowedState -> [a] -> [[Maybe a]]
myWindows LeftEdge l = (Nothing : map Just (take 2 l)) : myWindows Middle l
myWindows Middle l@(x:xs) = map Just (take 3 l) : myWindows (if length xs <= 2 then RightEdge else Middle) xs
myWindows RightEdge l = [map Just (take 2 l) ++ [Nothing]]
myWindows s l = debugMessage ("Dubious window: " ++ show s ++ show l) []


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

data GearEntry = NoGear | Gear | Number CountedInt deriving (Eq)
instance Show GearEntry where
  show NoGear = "_"
  show Gear = "*"
  show (Number CountedInt{value = x}) = show x
type GearSchematic = [[GearEntry]]

beautify = intercalate ",  " . map (unwords . repr)
repr = map reprEntry
reprEntry Gear = "*"
reprEntry (Number CountedInt{value = x}) = show x
reprEntry NoGear = "_"

emplaceNumbers = emplaceHitNumbers False [] 0

emplaceHitNumbers :: Bool -> String -> Int -> [SchematicEntry] -> [GearEntry] -- Current string hit -> NumberBuffer -> Index of number in line -> Remaining String -> Extracted Numbers
emplaceHitNumbers _ [] i (SchematicEntry{part = Char c, hit = hit}:sx) = emplaceHitNumbers hit [c] i sx
emplaceHitNumbers _ [] i (SchematicEntry{part = Symbol GearSymbol}:sx) = Gear : emplaceHitNumbers False [] i sx
emplaceHitNumbers _ [] i (s:sx) = NoGear : emplaceHitNumbers False [] i sx
emplaceHitNumbers alreadyHit b i (SchematicEntry{part = Char c, hit = hit}:sx) = emplaceHitNumbers (alreadyHit || hit) (b ++ [c]) i sx
emplaceHitNumbers hit b i (s:sx)
    | hit && isGear s = replicate (length b) (Number (CountedInt i (read b))) ++ Gear : emplaceHitNumbers False [] (i + 1) sx
    | hit = replicate (length b) (Number (CountedInt i (read b))) ++ NoGear : emplaceHitNumbers False [] (i + 1) sx
    | not hit && isGear s = replicate (length b) NoGear ++ Gear : emplaceHitNumbers False [] i sx
    | not hit = replicate (length b + 1) NoGear ++ emplaceHitNumbers False [] i sx
    where
        isGear SchematicEntry{part = Symbol GearSymbol} = True
        isGear _ = False
emplaceHitNumbers _ [] i [] = []
emplaceHitNumbers h b i []
    | h = replicate (length b) (Number (CountedInt i (read b)))
    | not h = []

condenseSquares :: [[GearEntry]] -> [[[GearEntry]]]
condenseSquares = map (map processSquare) . transpose . condense . transpose . condense
--condenseSquares = map (map processSquare) . debugMessageWith "Second Condense " showSquares . transpose . condense . transpose . condense
--condenseSquares = map (map processSquare) . condense . transpose . debugMessageWith "First Condense " showCondense . condense

condense :: Show a => [[a]] -> [[[Maybe a]]]
condense = map windowed

reduce :: [[[GearEntry]]] -> [[[GearEntry]]]
reduce = map (map (nub . removeNos))
    where
        removeNos (NoGear:xs) = removeNos xs
        removeNos (x:xs) = x : removeNos xs
        removeNos [] = []

processSquare :: [Maybe [Maybe GearEntry]] -> [GearEntry]
processSquare [x, Just y@[_, Just Gear, _], z] = (extract . filter numberFilter) (maybe [] nub x ++ nub y ++ maybe [] nub z)
    where
        numberFilter (Just (Number _)) = True
        numberFilter _ = False
processSquare _ = []

extract ((Just x):xs)= x : extract xs
extract (Nothing:xs)= extract xs
extract [] = []

showSquares :: [[[Maybe [Maybe GearEntry]]]] -> String
showSquares = intercalate "\n\n---\n\n" . map showLine
    where
        showLine = intercalate "\n\n" . map showSquare
        showSquare :: [Maybe [Maybe GearEntry]] -> String
        showSquare = intercalate "\n" . map (unwords . map show . extract) . extract

showCondense :: [[[Maybe GearEntry]]] -> String
showCondense = intercalate "\n" . map (intercalate "  " . map (intercalate "" . map showEntry))
    where
        showEntry Nothing = " "
        showEntry (Just x) = show x