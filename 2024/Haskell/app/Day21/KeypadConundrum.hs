module Day21.KeypadConundrum (solveDay21) where

import Control.Monad (liftM2)
import Data.Char (digitToInt)
import qualified Data.Either as Either
import Data.List (minimumBy, nub)
import Data.Map (Map, empty, fromList, insert, member, (!))
import Data.Maybe (catMaybes, mapMaybe)
import Data.Tuple (swap)
import Data.Tuple.HT (mapSnd)
import Debug.Trace (trace)
import GHC.Utils.Misc (nTimes, singleton)
import Util
import Prelude hiding (Left, Right)

solveDay21 input _ = do
    putStrLn "Day 21 - Keypad Conundrum:"

    -- let testSeq = [Either.Left Left,Either.Left Left,Either.Right ()]
    -- print $ findShortestSequence empty 1 testSeq
    -- let moves = zipWith (directionalMoves directionalPositions) (submit : testSeq) testSeq
    -- let moveSequences = map (map ((++[submit]) . map Either.Left)) moves
    -- print $ moveSequences
    -- print $ foldr (liftM2 (:)) [[]] moveSequences
    -- print $ liftM2 (:) [[Right,Right,Up]] [[]]
    -- print $ foldr x [[]] [testSeq]
    -- print $ nextSequences [Either.Left Left,Either.Left Left,Either.Right ()]
    print $ part1 input
    print $ part2 input

-- print $ findShortestSequence 1 [Either.Left Left, Either.Right ()]

-- print $ part1 input
-- print $ part2 input

readInput :: String -> [(Int, [Int])]
readInput = map (\x -> (read . init $ x, map digitToInt (init x) ++ [10])) . lines

part1 = sum . map (uncurry (*) . mapSnd (keypadToMinimumSequenceLength 2)) . readInput
part2 = sum . map (uncurry (*) . mapSnd (keypadToMinimumSequenceLength 25)) . readInput

keypadToMinimumSequenceLength indirections = minimum . map (sum . map (snd . findShortestSequenceLength empty indirections)) . keypadToSequences

type DirectionPadKey = Either Direction ()
submit = Either.Right () :: DirectionPadKey
type DirectionSequence = [DirectionPadKey]

type SequenceMemo = Map (Int, DirectionSequence) Int

showPath = map showDir
  where
    showDir (Either.Right ()) = 'A'
    showDir (Either.Left d) = showDirection d
    showDirection Left = '<'
    showDirection Right = '>'
    showDirection Up = '^'
    showDirection Down = 'v'

keypadToSequences :: [Int] -> [[DirectionSequence]]
keypadToSequences keys = foldr (liftM2 combine) [[]] $ zipWith (keypadMoves keypadPositions) (10 : keys) keys
  where
    combine dirs rem = (map Either.Left dirs ++ [submit]) : rem

findShortestSequenceLength :: SequenceMemo -> Int -> DirectionSequence -> (SequenceMemo, Int)
findShortestSequenceLength memo 0 seq = (memo, length seq)
findShortestSequenceLength memo indirectionCount seq
    | (indirectionCount, seq) `member` memo = (memo, memo ! (indirectionCount, seq))
    | otherwise = (insert (indirectionCount, seq) result resultMemo, result)
  where
    (resultMemo, result) = mapSnd minimum . lookupAll memo . nextSequences $ seq

    lookupNext :: SequenceMemo -> [DirectionSequence] -> (SequenceMemo, Int)
    lookupNext memo = foldr (\seq (memo, nexts) -> mapSnd (+ nexts) $ findShortestSequenceLength memo (indirectionCount - 1) seq) (memo, 0)

    lookupAll :: SequenceMemo -> [[DirectionSequence]] -> (SequenceMemo, [Int])
    lookupAll memo = foldr (\seqs (memo, remainings) -> mapSnd (: remainings) $ lookupNext memo seqs) (memo, [])
nextSequences :: DirectionSequence -> [[DirectionSequence]]
nextSequences seq = foldr (liftM2 (:) . map ((++ [submit]) . map Either.Left)) [[]] $ zipWith (directionalMoves directionalPositions) (submit : seq) seq

keypadPositions :: Map Int (Int, Int)
keypadPositions = fromList . map swap . concatMap (filter ((/= 11) . snd)) . xyEnumerate $ [[7, 8, 9], [4, 5, 6], [1, 2, 3], [11, 0, 10]]
keypadMoves positions a b
    | a `elem` [7, 4, 1] && b `elem` [0, 10] = [horizontal ++ vertical]
    | b `elem` [7, 4, 1] && a `elem` [0, 10] = [vertical ++ horizontal]
    | otherwise = nub [horizontal ++ vertical, vertical ++ horizontal]
  where
    (x1, y1) = positions ! a
    (x2, y2) = positions ! b
    horizontal = replicate (abs (x1 - x2)) (if x1 < x2 then Right else Left)
    vertical = replicate (abs (y1 - y2)) (if y1 < y2 then Down else Up)

directionalPositions = fromList . map swap . concatMap (mapMaybe (uncurry ((<$>) . (,)))) . xyEnumerate $ [[Nothing, Just (Either.Left Up), Just (Either.Right ())], map (Just . Either.Left) [Left, Down, Right]]
directionalMoves positions a b
    | a == Either.Left Left = [horizontal ++ vertical]
    | b == Either.Left Left = [vertical ++ horizontal]
    | otherwise = nub [horizontal ++ vertical, vertical ++ horizontal]
  where
    (x1, y1) = positions ! a
    (x2, y2) = positions ! b
    horizontal = replicate (abs (x1 - x2)) (if x1 < x2 then Right else Left)
    vertical = replicate (abs (y1 - y2)) (if y1 < y2 then Down else Up)
