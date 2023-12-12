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
countOptions (Record springs groups) = countRemainingOptions False springs (groups ++ [0])
    where
        --countRemainingOptions :: inGroup: Bool -> springs:[Spring] -> groups:[Int]
        countRemainingOptions :: Bool -> [Spring] -> [Int] -> Int
        countRemainingOptions False (Operational:xs) gs = countRemainingOptions False xs gs
        countRemainingOptions True (Operational:xs) (0:gs) = countRemainingOptions False xs gs
        countRemainingOptions True (Operational:xs) (_:gs) = 0

        countRemainingOptions False (Damaged:xs) (g:gs) = countRemainingOptions True xs (g-1 : gs)
        countRemainingOptions True (Damaged:xs) (0:gs) = 0
        countRemainingOptions True (Damaged:xs) (g:gs) = countRemainingOptions True xs (g-1:gs)

        countRemainingOptions ig (Unknown:xs) gs = countRemainingOptions ig (Operational:xs) gs + countRemainingOptions ig (Damaged:xs) gs

        countRemainingOptions ig [] z 
            | all (==0) z = 1
            | otherwise = 0

        countRemainingOptions x y z = debugMessagePlain ("x: " ++ show x ++ " y: " ++ show y ++ " z: " ++ show z) 0

unfold (Record springs groups) = Record (intercalate [Unknown] . replicate 5 $ springs) (concat . replicate 5 $ groups)