module Day10.Factory (solveDay10) where

import Data.List (transpose)
import Data.Tuple.HT (mapFst, mapSnd)
import Data.Tuple.Utils (dup)
import Text.Megaparsec (sepEndBy1, some, (<|>))
import Text.Megaparsec.Char (char, hspace, newline)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Util

solveDay10 input _ = do
    putStrLn "Day 10 - Factory:"
    machines <- parseOrError machinesParser input
    print . part1 $ machines

data Machine = Machine [Bool] [[Bool]] [Int] deriving (Show)

part1 = sum . map pressCount
  where
    pressCount (Machine goal buttons _) = snd . head . filter ((== goal) . fst) . map (mapSnd length . mapFst (map (foldr (/=) False) . transpose) . dup) . combinations $ buttons
    combinations buttons = [1 .. length buttons] >>= \n -> mapM (const buttons) [1 .. n]

machinesParser :: Parser [Machine]
machinesParser = machineParser `sepEndBy1` newline

machineParser :: Parser Machine
machineParser = combine <$> goalParser <* hspace <*> buttonParser `sepEndBy1` hspace <*> joltageParser
  where
    goalParser = char '[' *> some (True <$ char '#' <|> False <$ char '.') <* char ']' :: Parser [Bool]
    buttonParser = char '(' *> Lex.decimal `sepEndBy1` char ',' <* char ')'
    joltageParser = char '{' *> Lex.decimal `sepEndBy1` char ',' <* char '}'

    combine goal = Machine goal . map (\is -> [i `elem` is | i <- [0 .. length goal - 1]])
