module Day12.GardenGroups (solveDay12) where

import Control.Applicative ((<|>))
import Data.Array (Array, assocs, bounds, elems, inRange, (!), (//))
import Data.Either (fromLeft, isLeft)
import Data.Either.Utils (fromRight)
import Data.List (find, transpose)
import qualified Data.Map as Map
import Data.Map.Merge.Strict (dropMissing, merge, preserveMissing, zipWithMatched)
import Data.Map.Strict (Map, alter, empty, update)
import Data.Maybe (fromJust, isJust)
import Data.Set (Set, fromList, member, notMember)
import Data.Tuple.HT (mapSnd)
import GHC.Data.Maybe (orElse)
import Util
import qualified Data.Set as Set

solveDay12 input _ = do
    putStrLn "Day 12 - Garden Groups:"
    let lns = lines input
    print . part1 $ lns
    print . part2 $ lns

part1 lines = sum values
  where
    regions = floodRegions lines
    missingFences = countAllMissingFences regions
    plots = countAllPlots regions
    fences = merge preserveMissing dropMissing (zipWithMatched (const (-))) (fmap (* 4) plots) (fmap (* 2) missingFences)
    values = merge preserveMissing dropMissing (zipWithMatched (const (*))) plots fences

part2 = sum . fmap (\s -> cornerCount s * Set.size s) . toMap . floodRegions

countMissingFences :: (Eq a, Ord a) => Map a Int -> [a] -> Map a Int
countMissingFences counts [x] = counts
countMissingFences counts (x : y : zs)
    | x == y = countMissingFences (alter ((<|> Just 1) . fmap (+ 1)) x counts) (y : zs)
    | otherwise = countMissingFences counts (y : zs)

countAllMissingFences lines = foldr (flip countMissingFences) horizontalMissing (transpose lines)
  where
    horizontalMissing = foldr (flip countMissingFences) empty lines

countPlots :: (Eq a, Ord a) => Map a Int -> [a] -> Map a Int
countPlots = foldr (alter ((<|> Just 1) . fmap (+ 1)))

countAllPlots :: (Eq a, Ord a) => [[a]] -> Map a Int
countAllPlots = foldr (flip countPlots) empty

cornerCount coordSet = sum . map (Set.size . (`Set.filter` coordSet)) $ edgeTypes
    where
        tl (x,y) = (x+1,y) `member` coordSet && (x,y+1) `member` coordSet && (x+1,y+1) `notMember` coordSet
        tlInverted (x,y) = (x+1,y) `notMember` coordSet && (x,y+1) `notMember` coordSet
        bl (x,y) = (x+1,y) `member` coordSet && (x,y-1) `member` coordSet && (x+1,y-1) `notMember` coordSet
        blInverted (x,y) = (x+1,y) `notMember` coordSet && (x,y-1) `notMember` coordSet
        br (x,y) = (x-1,y) `member` coordSet && (x,y-1) `member` coordSet && (x-1,y-1) `notMember` coordSet
        brInverted (x,y) = (x-1,y) `notMember` coordSet && (x,y-1) `notMember` coordSet
        tr (x,y) = (x-1,y) `member` coordSet && (x,y+1) `member` coordSet && (x-1,y+1) `notMember` coordSet
        trInverted (x,y) = (x-1,y) `notMember` coordSet && (x,y+1) `notMember` coordSet

        edgeTypes = [tl, tlInverted, bl, blInverted, br, brInverted, tr, trInverted]

toMap :: [[Int]] -> Map Int (Set (Int, Int))
toMap regions = Map.fromList $ map (\n -> (n, coordSet n)) [0 .. (maxVal + 1)]
  where
    regionsMap = xyEnumerate regions
    coordSet n = fromList . map fst . concatMap (filter ((== n) . snd)) $ regionsMap
    maxVal = maximum . concat $ regions

floodRegions :: [String] -> [[Int]]
floodRegions = fromArray . fmap fromRight . floodRegions' 0 . gridToArray . map (map Left)
  where
    fromArray arr = map (\n -> map snd . filter ((== n) . snd . fst) . assocs $ arr) [0 .. (snd . snd . bounds $ arr)]

floodRegions' n arr
    | isJust regionStart' = floodRegions' (n + 1) (floodRegion (snd regionStart) n (fst regionStart) arr)
    | otherwise = arr
  where
    regionStart' = (find (isLeft . snd) . assocs) arr
    regionStart = mapSnd (fromLeft (error "not left")) (fromJust regionStart')

floodRegion :: Char -> Int -> (Int, Int) -> Array (Int, Int) (Either Char Int) -> Array (Int, Int) (Either Char Int)
floodRegion c n p@(x, y) a
    | not (inRange (bounds a) p) = a
    | fromLeft '.' (a ! p) /= c = a
    | otherwise =
        floodRegion c n (x + 1, y)
            . floodRegion c n (x - 1, y)
            . floodRegion c n (x, y + 1)
            . floodRegion c n (x, y - 1)
            $ a // [(p, Right n)]
