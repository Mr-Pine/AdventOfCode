module Day12.HotSprings (solveDay12) where
import Util (Parser, example, parseOrError, input, debugMessagePlain)
import Text.Megaparsec.Char (char, newline, space)
import Text.Megaparsec ((<|>), many, sepBy, sepEndBy, endBy)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List (intercalate)

solveDay12 = do
    putStrLn "Day 12 - Hot Springs:"
    input <- input 12
    records <- parseOrError recordsParser input
    print . part1 $ records
    print . part2 $ records

data Spring = Operational | Damaged | Unknown deriving (Show, Eq)
data Record = Record [Spring] [Int] deriving (Show, Eq)

part1 = sum . map countOptions
part2 = sum . map (countOptions . unfold)

recordsParser :: Parser [Record]
recordsParser = recordParser `endBy` newline
    where
        recordParser = Record <$> springParser <* space <*> groupParser
        spring = Operational <$ char '.' <|> Damaged <$ char '#' <|> Unknown <$ char '?'
        springParser = many spring
        groupParser = L.decimal `sepBy` char ','

countOptions :: Record -> Int
countOptions (Record springs groups) = countRemainingOptions springs groups
    where
        --countRemainingOptions :: springs:[Spring] -> groups:[Int]
        countRemainingOptions :: [Spring] -> [Int] -> Int
        countRemainingOptions (Operational:xs) gs = countRemainingOptions xs gs
        countRemainingOptions (Unknown:xs) gs = countRemainingOptions (Operational:xs) gs + countRemainingOptions (Damaged:xs) gs

        countRemainingOptions x@(Damaged:xs) y@(g:gs)
            | noOperational && (noNext || nextNotDamaged) = countRemainingOptions (drop g xs) gs
            | otherwise = 0
            where
                noOperational = notElem Operational . take (g - 1) $ xs
                noNext = length xs == g - 1
                nextNotDamaged = length xs >= g && xs!!(g-1) /= Damaged

        countRemainingOptions [] [] = 1
        countRemainingOptions x y = 0

unfold (Record springs groups) = Record (intercalate [Unknown] . replicate 5 $ springs) (concat . replicate 5 $ groups)