module Day17.ChronospatialComputer (solveDay17) where

import Control.Monad.State (MonadState (get, put), State, evalState, gets, modify, runState)
import Data.Array (Array, bounds, inRange, listArray, (!))
import Data.Bits (xor)
import Data.List (intercalate)
import Debug.Trace (trace, traceShow)
import Text.Megaparsec (anySingle, sepEndBy1, skipManyTill)
import Text.Megaparsec.Char (char, newline, string)
import Util

solveDay17 input _ = do
    putStrLn "Day 17 - Chronospatial Computer"
    computer <- parseOrError computerParser input
    programBytes <- parseOrError programParser input
    print . part1 $ computer
    print $ part2 programBytes computer

part1 = intercalate "," . map show . evalState eval

part2 target computer = minimum (findInputs target)
  where
    findInputs :: [Int] -> [Int]
    findInputs [] = [0]
    findInputs tss@(t : ts) = filter ((== tss) . reverse . take (length tss) . reverse . (\n -> evalState (evalWithA n) computer)) . combine (findInputs ts) $ [0 .. 7]

    combine :: [Int] -> [Int] -> [Int]
    combine previousInputs = concatMap (\x -> map ((+ x) . (* 8)) previousInputs)

evalWithA a = do
    modify (\state -> state{a = a})
    eval

data ComputerState = ComputerState {instructions :: Array Int Instruction, ip :: Int, a :: Int, b :: Int, c :: Int} deriving (Show)

data ComboOperand = Literal Int | A | B | C

combo 4 = A
combo 5 = B
combo 6 = C
combo n
    | n >= 0 && n <= 3 = Literal n

evalOperand :: ComboOperand -> State ComputerState Int
evalOperand (Literal n) = return n
evalOperand A = gets a
evalOperand B = gets b
evalOperand C = gets c

data Operation = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV deriving (Enum, Show)

fromOpcode :: Int -> Operation
fromOpcode = toEnum

runOp ADV op = do
    opVal <- evalOperand (combo op)
    aVal <- gets a
    let result = aVal `div` (2 ^ opVal)
    modify (\state -> state{a = result})
    return Nothing
runOp BXL op = do
    bVal <- gets b
    modify (\state -> state{b = bVal `xor` op})
    return Nothing
runOp BST op = do
    opVal <- evalOperand (combo op)
    modify (\state -> state{b = opVal `mod` 8})
    return Nothing
runOp JNZ op = do
    aVal <- gets a
    ipTarget <- if aVal == 0 then gets ip else evalOperand (combo op)
    modify (\state -> state{ip = ipTarget})
    return Nothing
runOp BXC _ = do
    bVal <- gets b
    cVal <- gets c
    modify (\state -> state{b = bVal `xor` cVal})
    return Nothing
runOp OUT op = do
    opVal <- evalOperand (combo op)
    return (Just (opVal `mod` 8))
runOp BDV op = do
    opVal <- evalOperand (combo op)
    aVal <- gets a
    let result = aVal `div` (2 ^ opVal)
    modify (\state -> state{b = result})
    return Nothing
runOp CDV op = do
    opVal <- evalOperand (combo op)
    aVal <- gets a
    let result = aVal `div` (2 ^ opVal)
    modify (\state -> state{c = result})
    return Nothing

data Instruction = Instruction Operation Int deriving (Show)

runInstruction :: Instruction -> State ComputerState (Maybe Int)
runInstruction (Instruction op arg) = runOp op arg

evalCurrentIp :: State ComputerState (Maybe Int)
evalCurrentIp = do
    instructionPointer <- gets ip
    instruction <- gets ((! instructionPointer) . instructions)
    modify (\state -> state{ip = instructionPointer + 1})

    runInstruction instruction

eval :: State ComputerState [Int]
eval = do
    instructionBounds <- gets (bounds . instructions)
    instructionPointer <- gets ip
    if not (inRange instructionBounds instructionPointer)
        then
            return []
        else do
            result <- evalCurrentIp
            remainder <- eval
            return (maybe remainder (: remainder) result)

computerParser :: Parser ComputerState
computerParser = flip fillRegisters <$> registerParser <* newline <*> instructionState
  where
    registerParser = (string "Register " *> anySingle *> string ": " *> number) `sepEndBy1` newline
    instructionsParser = string "Program: " *> ((Instruction <$> (fromOpcode <$> number) <* char ',' <*> number) `sepEndBy1` char ',')
    toArray insns = listArray (0, length insns - 1) insns
    instructionState = ComputerState . toArray <$> instructionsParser <*> pure 0
    fillRegisters f [a, b, c] = f a b c

programParser :: Parser [Int]
programParser = skipManyTill anySingle (string "Program: ") *> (number `sepEndBy1` char ',')
