module Day1.SecretEntrance (solveDay1) where
import Util
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec ( sepEndBy, (<|>))
import Text.Megaparsec.Char

solveDay1 input _ = do
    putStrLn "Day 1 - Secret Entrance:"
    rotations <- parseOrError rotationsParser input
    print $ part1 rotations

part1 = length . filter (== 0) . collectedRotations

data Rotation = L Int | R Int deriving (Show, Eq)

numeric (L x) = -x
numeric (R x) = x

collectedRotations :: [Rotation] -> [Int]
collectedRotations = scanl (\a x ->  (a + numeric x) `mod` 100) 50

rotationsParser :: Parser [Rotation]
rotationsParser = rotationParser `sepEndBy` newline

rotationParser :: Parser Rotation
rotationParser = (L <$> (char 'L' *> Lex.decimal)) <|> (R <$> (char 'R' *> Lex.decimal))
