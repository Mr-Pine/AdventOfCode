module Day10.Factory (solveDay10) where

import Data.Foldable (foldr', traverse_)
import Data.Maybe (fromJust)
import Data.SBV (ConstraintSet, Modelable (extractModel, getModelValue), OptimizeResult, OptimizeStyle (Lexicographic), SInteger, SMTConfig (verbose), SymVal (literal), SymbolicT, constrain, fromBool, minimize, oneIf, optLexicographic, optLexicographicWith, optimize, optimizeWith, sAnd, sBool, sFromIntegral, sInteger, z3, (.&&), (.<+>), (.==), (.>=))
import Text.Megaparsec (sepEndBy1, some, (<|>))
import Text.Megaparsec.Char (char, hspace, newline)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Util

solveDay10 input _ = do
    putStrLn "Day 10 - Factory:"
    machines <- parseOrError machinesParser input
    print =<< part1 machines
    print =<< part2 machines


data Machine = Machine [Bool] [[Int]] [Integer] deriving (Show)

part1 = getPressCounts part1PressCount

part1PressCount (Machine target buttons _) = do
    buttonPressedStates <- mapM (sBool . ("buttonPressed" <>) . show) [0 .. (length buttons - 1)]
    let sTarget = map fromBool target
    let sButtons = map (map fromBool . buttonToBools (length target)) buttons

    let pressedButtons = zipWith (map . (.&&)) buttonPressedStates sButtons
    let buttonResult = foldr1 (zipWith (.<+>)) pressedButtons

    constrain (sAnd (zipWith (.==) buttonResult sTarget))

    let pressCount = sum $ map oneIf buttonPressedStates :: SInteger

    return pressCount
  where
    buttonToBools n button = [i `elem` button | i <- [0 .. n - 1]]

part2 = getPressCounts part2PressCount

part2PressCount (Machine _ buttons target) = do
    buttonPressedCounts <- mapM (sInteger . ("buttonPressed" <>) . show) [0 .. (length buttons - 1)]

    traverse_ (constrain . (.>= literal 0)) buttonPressedCounts

    let sTarget = map literal target
    let sButtons = map (buttonToBools (length target)) buttons

    let pressedButtons = zipWith (map . (*)) buttonPressedCounts sButtons
    let buttonResult = foldr1 (zipWith (+)) pressedButtons

    constrain (sAnd (zipWith (.==) buttonResult sTarget))

    let pressCount = sum buttonPressedCounts

    return pressCount
  where
    buttonToBools n button = [oneIf . fromBool $ (i `elem` button) | i <- [0 .. n - 1]] :: [SInteger]

getPressCounts :: (Machine -> SymbolicT IO SInteger) -> [Machine] -> IO Integer
--getPressCounts :: (Machine -> SymbolicT IO SInteger) -> [Machine] -> IO OptimizeResult
getPressCounts counter = fmap (fromJust . getModelValue "totalPressCount") . optLexicographic {-optimizeWith z3{verbose=True} Lexicographic-} . pressCount
  where
    pressCount machines = minimize "totalPressCount" . sum =<< mapM counter machines

machinesParser :: Parser [Machine]
machinesParser = machineParser `sepEndBy1` newline

machineParser :: Parser Machine
machineParser = Machine <$> goalParser <* hspace <*> buttonParser `sepEndBy1` hspace <*> joltageParser
  where
    goalParser = char '[' *> some (True <$ char '#' <|> False <$ char '.') <* char ']' :: Parser [Bool]
    buttonParser = char '(' *> Lex.decimal `sepEndBy1` char ',' <* char ')'
    joltageParser = char '{' *> Lex.decimal `sepEndBy1` char ',' <* char '}'
