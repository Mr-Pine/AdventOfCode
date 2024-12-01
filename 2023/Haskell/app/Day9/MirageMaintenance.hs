module Day9.MirageMaintenance (solveDay9) where
import Util (example, Parser, parseOrError, input)
import Text.Megaparsec.Char (newline, hspace)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec (sepEndBy, sepBy)

solveDay9 = do
    putStrLn "Day 9 - Mirage Maintenance"
    input <- input 9
    sequences <- parseOrError sequencesParser input
    print $ part1 sequences
    print $ part2 sequences
    
part1 = sum . map nextValue
part2 = sum . map previousValue

sequencesParser :: Parser [[Int]]
sequencesParser = sequence `sepEndBy` newline
    where
        sequence = L.signed hspace L.decimal `sepBy` hspace :: Parser [Int]

expandValue :: ([Int] -> Int -> Int) -> [Int] -> Int
expandValue expandWith sequence 
    | all (==0) sequence = 0
    | otherwise = expandWith sequence (expandValue expandWith (differences sequence))
        where
            differences = zipWith (-) =<< tail

nextValue = expandValue (\seq n -> last seq + n)
previousValue = expandValue (\seq n -> head seq - n)