module Day18.RamRun (solveDay18) where

import Util
import Data.Set (member, insert, fromList, deleteFindMin, union, empty)
import Data.Foldable (minimumBy)
import Text.Megaparsec.Char (newline, char)
import Text.Megaparsec (sepEndBy1)
import Data.Sequence (Seq(Empty, (:<|), (:|>)), singleton, (|>), (><), (<|))
import Data.Array (inRange)
import qualified Data.Sequence as Sequence
import Debug.Trace (trace)
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing, fromJust)

solveDay18 input isExample = do
    putStrLn "Day 18 - RAM Run:"
    let target = if isExample then (6,6) else (70,70)
    let byteCount = if isExample then 12 else 1024
    bytes <- parseOrError byteParser input
    --print target
    --print bytes
    print . fromJust $ part1 byteCount target bytes
    print $ part2 target bytes

part1 byteCount target bts = bfs target ((0,0),target) memory (Sequence.fromList [Set.singleton (0,0), empty])
    where
        memory = bytesFall byteCount bts

part2 target bts = (\(x,y) -> show x ++ ',' : show y) . (bts !!) . subtract 1 $ binarySearch (\n -> isNothing (part1 n target bts)) 0 (uncurry (*) target)

data QueueEntry a = Node a | Separator deriving (Show)

bfs target bounds visited Empty = Just 0
bfs _ _ _ seq | seq == Sequence.fromList [empty, empty] = Nothing
bfs target bounds visited (head :<| rest) | null head = (1 +) <$> bfs target bounds visited (rest |> empty)
bfs target bounds visited seq@(current :<| (rest :|> appendSet))
    | pos == target = Just 0
    | otherwise = bfs target bounds (pos `insert` visited) newQueue
    where
        (pos, remainder) = deleteFindMin current
        newNodes = filter (inRange bounds) . filter (not . (`member` visited)) . map (`moveInDirection` pos) $ directions
        newQueue = (remainder <| rest) |> (appendSet `union` fromList newNodes)

binarySearch p lower upper
    | lower == upper = lower
    | otherwise = binarySearch p (if middlePasses then lower else midpoint + 1) (if middlePasses then midpoint else upper)
    where
        midpoint = (lower + upper) `div` 2
        middlePasses = p midpoint


bytesFall n = fromList . take n

byteParser :: Parser [(Int,Int)]
byteParser = ((,) <$> number <* char ',' <*> number) `sepEndBy1` newline
