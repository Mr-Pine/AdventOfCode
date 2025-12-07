module Day6.TrashCompactor (solveDay6) where

import Data.List (dropWhileEnd, transpose)
import Data.String.HT (trim)
import Text.Megaparsec (sepBy, sepEndBy1, (<|>))
import Text.Megaparsec.Char (char, hspace, hspace1, newline)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Util

solveDay6 input _ = do
    putStrLn "Day 6 - Trash Compactor:"
    problems <- parseOrError problemsParser input
    print . worksheetResult $ problems
    cProblems <- cephalopodProblems input
    print . worksheetResult $ cProblems

worksheetResult = sum . map result

data Operation = Sum | Product deriving (Show)
func Sum = sum
func Product = product
data Problem = Problem [Int] Operation deriving (Show)

result (Problem values op) = func op values

valuesParser :: Parser [[Int]]
valuesParser = transpose <$> ((hspace *> Lex.decimal `sepEndBy1` hspace1) `sepEndBy1` newline)

cephalopodProblems input = do
    let values = map (map read) . splitOn "" . map trim . transpose . init . lines $ input
    ops <- parseOrError operationsParser (last . lines $ input)
    pure (zipWith Problem values ops)

operationsParser :: Parser [Operation]
operationsParser = (Sum <$ char '+' <|> Product <$ char '*') `sepEndBy1` hspace1

problemsParser :: Parser [Problem]
problemsParser = zipWith Problem <$> valuesParser <*> operationsParser
