module Day5.Cafeteria (solveDay5) where

import qualified Text.Megaparsec.Char.Lexer as Lex
import Util
import Text.Megaparsec.Char (newline, char)
import Text.Megaparsec (sepEndBy1)

solveDay5 input _ = do
    putStrLn "Day 5 - Cafeteria:"
    inventory <- parseOrError inventoryParser input
    print . uncurry part1 $ inventory

part1 ranges = length . filter (flip any ranges . flip contains)

type IngredientId = Int

data IdRange = IdRange IngredientId IngredientId deriving Show
contains (IdRange lower upper) i = i >= lower && i <= upper

idParser :: Parser IngredientId
idParser = Lex.decimal

idRangesParser :: Parser [IdRange]
idRangesParser = (IdRange <$> idParser <* char '-' <*> idParser) `sepEndBy1` newline

inventoryParser :: Parser ([IdRange], [IngredientId])
inventoryParser = (,) <$> idRangesParser <* newline <*> (idParser `sepEndBy1` newline)
