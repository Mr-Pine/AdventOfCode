module Day13.PointOfIncidence (solveDay13) where
import Util (example, input, Parser, parseOrError, debugMessage, debugMessagePlain)
import Text.Megaparsec (many, (<|>), sepBy, sepEndBy, sepBy1, some, sepEndBy1)
import Text.Megaparsec.Char (char, newline, space)
import Data.Foldable (find)
import Data.Maybe (fromJust, mapMaybe)
import Data.List (transpose)

solveDay13 = do
    putStrLn "Day 13 - Point of Incidence"
    input <- input 13
    maps <- parseOrError mapsParser input
    print . part1 $ maps
    print . part2 $ maps

part1 maps = vertical + 100 * horizontal
    where
        vertical = sum $ mapMaybe findVerticalMirror maps
        horizontal = sum $ mapMaybe findHorizontalMirror maps

part2 maps = vertical + 100 * horizontal
    where
        vertical = sum $ mapMaybe findSmudgedVerticalMirror maps
        horizontal = sum $ mapMaybe findSmudgedHorizontalMirror maps

mapsParser :: Parser [[[Terrain]]]
mapsParser = mapParser `sepBy` space

mapParser :: Parser [[Terrain]]
mapParser = terrainParser `sepEndBy1` newline

terrainParser :: Parser [Terrain]
terrainParser = some (Ash <$ char '.' <|> Rock <$ char '#')

data Terrain = Ash | Rock deriving (Show, Eq)

findHorizontalMirror :: [[Terrain]] -> Maybe Int
findHorizontalMirror map = find isMirror indices
    where
        isMirror i = and $ zipWith (==) (drop i map) (reverse $ take i map)
        indices = [1..length map - 1]

findVerticalMirror = findHorizontalMirror . transpose

data MirrorImage = Perfect | Smudged | None deriving (Show, Eq)
findSmudgedHorizontalMirror map = find isMirror indices
    where
        isMirror i = (\as -> count Smudged as == 1 && count None as == 0) $ zipWith checkMirrorImage (drop i map) (reverse $ take i map)
        checkMirrorImage :: [Terrain] -> [Terrain] -> MirrorImage
        checkMirrorImage = (((\n -> if n==0 then Perfect else if n == 1 then Smudged else None) . count False) .) . zipWith (==)
        count a = length . filter (==a)
        indices = [1..length map - 1]

findSmudgedVerticalMirror = findSmudgedHorizontalMirror . transpose