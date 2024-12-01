module Day20.PulsePropagation (solveDay20) where
import Util (Parser, example, parseOrError, input)
import Text.Megaparsec.Char (string, char, alphaNumChar, space)
import Text.Megaparsec ((<|>), some, sepBy, sepEndBy)
import Data.List (singleton)

solveDay20 = do
    putStrLn "Day 20 - Pulse Propagation:"
    input <- example 20
    modules <- parseOrError modulesParser input
    print modules

data ModuleType = Broadcaster | FlipFlop String | Conjunction String deriving (Show, Eq)
data Module = Module ModuleType [String] deriving (Show, Eq)
data Pulse = Low | High deriving (Show, Eq)

identifier Broadcaster = "broadcaster"
identifier (FlipFlop s) = s
identifier (Conjunction s) = s

moduleTypeParser :: Parser ModuleType
moduleTypeParser = Broadcaster <$ string "broadcaster" <|> FlipFlop <$> (char '%' *> some alphaNumChar) <|> Conjunction <$> (char '&' *> some alphaNumChar)
moduleParser :: Parser Module
moduleParser = Module <$> moduleTypeParser <* string " -> " <*> (some alphaNumChar `sepBy` string ", ")
modulesParser :: Parser [Module]
modulesParser = moduleParser `sepEndBy` space