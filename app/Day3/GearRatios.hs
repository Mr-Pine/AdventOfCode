module Day3.GearRatios where
import Util (example, Parser, input, parseOrError)
import Text.Megaparsec.Char (string, char, space, newline, digitChar)
import Text.Megaparsec (sepBy1, (<|>), endBy1, many)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Text (pack)
import Data.Either (isLeft, lefts, rights)
import Data.List (transpose, intercalate)
import Debug.Trace (trace)

solveDay3 = do
    putStrLn "Day 3 - Gear Ratios:"
    input <- example 3
    schematic <- parseOrError parseSchematic input
    putStrLn $ (intercalate "\n"  . (getHit . expand)) schematic

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
        parseSymbol = (`SchematicEntry` True) <$> (Symbol <$ (char '*' <|> char '+' <|> char '#' <|> char '$'))

expandHorizontally :: [SchematicEntry] -> [SchematicEntry]
expandHorizontally (SchematicEntry{part = xVal}:y@SchematicEntry{part = yVal, hit = hit}:z@SchematicEntry{part = zVal, hit = zHit}:rest)
    | hit && not zHit = SchematicEntry xVal True : SchematicEntry yVal True : SchematicEntry zVal True : expandHorizontally rest
    | hit && zHit = SchematicEntry xVal True : expandHorizontally (y : z : rest)
    | not hit = SchematicEntry xVal False : expandHorizontally (y : z : rest)
expandHorizontally [SchematicEntry{part = xVal}, y@SchematicEntry{hit = hit}] = [SchematicEntry xVal hit,  y]
expandHorizontally [x] = [x]
expandHorizontally [] = []

--expand = transpose . map expandHorizontally . transpose . map expandHorizontally
expand = transpose . transpose . map expandHorizontally

getHit = map (map ((\bool -> if bool then '1' else '0') . hit))

extractNumbersFromSchematic :: Schematic -> [Int]
extractNumbersFromSchematic = concatMap extractNumbers

extractNumbers :: [SchematicEntry] -> [Int]
extractNumbers = extractHitNumbersLog False []

extractHitNumbersLog :: Bool -> String -> [SchematicEntry] -> [Int]
extractHitNumbersLog b s ses = trace ("Current string hit: " ++ show b ++ " buffer: " ++ s ++ " entries: " ++ show ses) (extractHitNumbers b s ses)

extractHitNumbers :: Bool -> String -> [SchematicEntry] -> [Int] -- Current string hit -> NumberBuffer -> Remaining String -> Extracted Numbers
extractHitNumbers _ [] (SchematicEntry{part = Char c, hit = hit}:sx) = extractHitNumbersLog False [c] sx
extractHitNumbers _ [] (s:sx) = extractHitNumbersLog False [] sx
extractHitNumbers alreadyHit b (SchematicEntry{part = Char c, hit = hit}:sx) = extractHitNumbersLog (alreadyHit || hit) (b ++ [c]) sx
extractHitNumbers hit b (s:sx)
    | hit = read b : extractHitNumbersLog False [] sx
    | not hit = extractHitNumbersLog False [] sx
extractHitNumbers _ _ [] = []