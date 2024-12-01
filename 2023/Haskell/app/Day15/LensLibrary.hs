module Day15.LensLibrary (solveDay15) where
import Data.Char (ord)
import Data.List.Split (splitOn)
import Util (example, input, Parser, parseOrError)
import qualified Data.Map.Ordered as Map
import Text.Megaparsec.Char (alphaNumChar, char)
import Text.Megaparsec (many, (<|>), sepBy)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Array (listArray, Array, (!), (//), elems)
import Data.Foldable (Foldable(toList))

solveDay15 = do
    putStrLn "Day 15 - Lens Library:"
    input <- input 15
    print . part1 $ input
    steps <- parseOrError stepsParser input
    print . part2 $ steps

part1 = sum . map hash . steps
part2 = focusingPower . processSteps boxes

steps :: String -> [String]
steps = map (filter (/='\n')) . splitOn ","

hash :: String -> Int
hash = foldl nextHash 0
    where
        nextHash a c = (a + ord c) * 17 `rem` 256

type Box = Map.OMap String Int

data Operation = Insert Int | Remove deriving (Show, Eq)
data Step = Step String Operation deriving (Show, Eq)

stepsParser :: Parser [Step]
stepsParser = stepParser `sepBy` char ','
stepParser :: Parser Step
stepParser = Step <$> many alphaNumChar <*> operationParser
    where
        operationParser = Remove <$ char '-' <|> Insert <$> (char '=' *> L.decimal)

boxes = listArray (0, 255) (replicate 256 Map.empty) :: Array Int Box

processSteps :: Array Int Box -> [Step] -> Array Int Box
processSteps = foldl doStep
    where
        doStep boxes (Step label operation) = boxes // [(arrIndex, newMap operation)]
            where
                newMap Remove = Map.delete label (boxes!arrIndex)
                newMap (Insert f) = (boxes!arrIndex) Map.|> (label, f)
                arrIndex = hash label

focusingPowerBox :: Box -> Int
focusingPowerBox = sum . zipWith (*) [1..] . toList

focusingPower :: Array Int Box -> Int
focusingPower = sum . zipWith (*) [1..] . map focusingPowerBox . toList