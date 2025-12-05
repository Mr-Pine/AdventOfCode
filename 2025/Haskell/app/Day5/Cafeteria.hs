module Day5.Cafeteria (solveDay5) where

import qualified Text.Megaparsec.Char.Lexer as Lex
import Util
import Text.Megaparsec.Char (newline, char)
import Text.Megaparsec (sepEndBy1)
import Data.List.Utils (uniq)
import Data.Range (Range (..), (+=+), inRanges, fromRanges, mergeRanges, Bound (..), BoundType (..))

solveDay5 input _ = do
    putStrLn "Day 5 - Cafeteria:"
    inventory <- parseOrError inventoryParser input
    print . uncurry part1 $ inventory
    print . uncurry (const . part2) $ inventory

part1 ranges = length . filter (inRanges ranges)
part2 = sum . map entryCount . mergeRanges

type IngredientId = Int
type IdRange = Range IngredientId
entryCount (SingletonRange _) = 1
entryCount (SpanRange (Bound x Inclusive) (Bound y Inclusive)) = y - x + 1

idParser :: Parser IngredientId
idParser = Lex.decimal

idRangesParser :: Parser [IdRange]
idRangesParser = ((+=+) <$> idParser <* char '-' <*> idParser) `sepEndBy1` newline

inventoryParser :: Parser ([IdRange], [IngredientId])
inventoryParser = (,) <$> idRangesParser <* newline <*> (idParser `sepEndBy1` newline)
