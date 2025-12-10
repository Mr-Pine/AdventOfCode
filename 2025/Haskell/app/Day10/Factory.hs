module Day10.Factory (solveDay10) where

import Data.Foldable (foldr')
import Data.Maybe (fromJust)
import Data.SBV (ConstraintSet, Modelable (extractModel, getModelValue), OptimizeResult, OptimizeStyle (Lexicographic), SInteger, SMTConfig (verbose), constrain, fromBool, minimize, oneIf, optLexicographic, optLexicographicWith, optimize, optimizeWith, sAnd, sBool, z3, (.&&), (.<+>), (.==))
import Text.Megaparsec (sepEndBy1, some, (<|>))
import Text.Megaparsec.Char (char, hspace, newline)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Util

solveDay10 input _ = do
    putStrLn "Day 10 - Factory:"
    machines <- parseOrError machinesParser input
    part1Res <- part1 machines
    print part1Res
    -- print . part2 $ machines

data Machine = Machine [Bool] [[Int]] [Int] deriving (Show)

part1 :: [Machine] -> IO Integer
part1 = fmap sum . mapM pressCount
  where
    pressCount = fmap (fromJust . getModelValue "pressCount") . optLexicographic . pressCountConstraints
    pressCountConstraints (Machine target buttons _) = do
        buttonPressedStates <- mapM (sBool . ("buttonPressed" <>) . show) [0 .. (length buttons - 1)]
        let sTarget = map fromBool target
        let sButtons = map (map fromBool . buttonToBools (length target)) buttons

        let pressedButtons = zipWith (map . (.&&)) buttonPressedStates sButtons
        let buttonResult = foldr1 (zipWith (.<+>)) pressedButtons

        constrain (sAnd (zipWith (.==) buttonResult sTarget))

        let pressCount = sum $ map oneIf buttonPressedStates :: SInteger

        minimize "pressCount" pressCount

    buttonToBools n button = [i `elem` button | i <- [0 .. n - 1]]

machinesParser :: Parser [Machine]
machinesParser = machineParser `sepEndBy1` newline

machineParser :: Parser Machine
machineParser = Machine <$> goalParser <* hspace <*> buttonParser `sepEndBy1` hspace <*> joltageParser
  where
    goalParser = char '[' *> some (True <$ char '#' <|> False <$ char '.') <* char ']' :: Parser [Bool]
    buttonParser = char '(' *> Lex.decimal `sepEndBy1` char ',' <* char ')'
    joltageParser = char '{' *> Lex.decimal `sepEndBy1` char ',' <* char '}'
