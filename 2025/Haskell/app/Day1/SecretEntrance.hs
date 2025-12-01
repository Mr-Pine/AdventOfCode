module Day1.SecretEntrance (solveDay1) where

import Text.Megaparsec (sepEndBy, (<|>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Util

solveDay1 input _ = do
    putStrLn "Day 1 - Secret Entrance:"
    rotations <- parseOrError rotationsParser input
    print $ part1 rotations
    print $ part2Dumb rotations

-- print $ part2 rotations

part1 = length . filter (== 0) . collectedRotations

part2Dumb = part1 . expand
  where
    expand = concatMap expandSingle
    expandSingle (L n) = replicate n (L 1)
    expandSingle (R n) = replicate n (R 1)

{-
part2 = snd . collect where
    collect = foldl rotationStep (50, 0)
    rotationStep (currentRotation, zeroCount) rot = (newRot `mod` 100, zeroCount + newZeroCount currentRotation newRot) where
        newRot = currentRotation + (numeric rot)
        newZeroCount current new = if (new < 0) then 1 + newZeroCount current (new + 100) else max (new `div` 100) 0
-}

data Rotation = L Int | R Int deriving (Show, Eq)

numeric (L x) = -x
numeric (R x) = x

collectedRotations :: [Rotation] -> [Int]
collectedRotations = scanl (\a x -> (a + numeric x) `mod` 100) 50

rotationsParser :: Parser [Rotation]
rotationsParser = rotationParser `sepEndBy` newline

rotationParser :: Parser Rotation
rotationParser = (L <$> (char 'L' *> Lex.decimal)) <|> (R <$> (char 'R' *> Lex.decimal))
