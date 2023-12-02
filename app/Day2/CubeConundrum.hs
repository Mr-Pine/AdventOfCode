module Day2.CubeConundrum where
import Util (example, Parser, input, parseOrError)
import Text.Megaparsec.Char (string, char, space, newline)
import Text.Megaparsec (sepBy1, (<|>), endBy1, parse, match, errorBundlePretty, Stream)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Text (pack)
import Data.Either (isLeft, lefts, rights)

solveDay2 = do
    putStrLn "Day 2 - Cube Conundrum:"
    input <- input 2
    games <- parseOrError parseGames input
    print $ part1 games
    print $ part2 games

part2 :: [Game] -> Int
part2 = sum . map power

power :: Game -> Int
power game = colorMax red game * colorMax green game * colorMax blue game

colorMax :: (Subset -> Int) -> Game -> Int
colorMax color game = maximum (map color (subsets game))

part1 :: [Game] -> Int
part1 (x:xs) | part1Fits x = game_id x + part1 xs
part1 (x:xs) = part1 xs
part1 [] = 0

part1Fits :: Game -> Bool
part1Fits = all (\subset -> red subset <= 12 && green subset <= 13 && blue subset <= 14) . subsets

data Color = Red | Green | Blue deriving (Eq, Enum)

data Subset = Subset {
    red :: Int,
    green :: Int,
    blue :: Int
} deriving Show

data Game = Game {
    game_id :: Int,
    subsets :: [Subset]
} deriving Show

parseGames :: Parser [Game]
parseGames = parseGame `endBy1` newline

parseGame :: Parser Game
parseGame = Game <$> id <*> (parseSubset `sepBy1` (char ';' *> space))
    where
        id = string "Game" *> space *> decimal <* char ':' <* space :: Parser Int

parseSubset :: Parser Subset
parseSubset = parsePairs <$> ((,) <$> decimal <* space <*> parseColor) `sepBy1` (char ',' *> space)

parsePairs :: [(Int, Color)] -> Subset
parsePairs pairs = Subset (getColor Red pairs) (getColor Green pairs) (getColor Blue pairs)

getColor :: Color -> [(Int, Color)] -> Int
getColor _ [] = 0
getColor color ((i, color_p):xs) | color == color_p = i
getColor color (x:xs) = getColor color xs

parseColor :: Parser Color
parseColor = Red <$ string "red" <|> Green <$ string "green" <|> Blue <$ string "blue"