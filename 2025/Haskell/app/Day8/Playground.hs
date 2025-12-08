module Day8.Playground (solveDay8) where

import Control.Monad (foldM, foldM_, liftM2)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, getElems, newArray, newListArray, readArray, writeArray)
import Data.List (sort, sortBy, tails, uncons)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (Down (Down), comparing)
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (char, newline)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Util
import Data.Tuple.Utils (fst3)

solveDay8 input isExample = do
    putStrLn "Day 8 - Playground:"
    junctions <- parseOrError junctionParser input
    print . part1 isExample $ junctions
    print . part2 $ junctions

part1 isExample junctions = product . take 3 . sortBy (comparing Data.Ord.Down) . catMaybes $ runST unioned
  where
    pairCount = if isExample then 10 else 1000
    joiningPairs = take pairCount . map snd $ sortedDistances junctions
    unioned = do
        uf <- initUnionFind (length junctions)
        foldM_ (uncurry . union') uf joiningPairs
        rootSizes uf

part2 junctions = uncurry (*) . tmap (fst3 . (junctions !!)) $ runST findLastNeededJoin
  where
    joins = map snd $ sortedDistances junctions
    findLastNeededJoin = findLastNeededJoin' joins =<< initUnionFind (length junctions)
    findLastNeededJoin' (j@(p, q) : js) uf = do
        newSize <- union uf p q
        if newSize == length junctions
            then return j
            else do
                findLastNeededJoin' js uf

sortedDistances junctions = sort . concat $ idDistances
  where
    idDistances = zipWith zip (distances junctions) ids
    distances = map (uncurry (map . distance)) . mapMaybe uncons . tails
    distance (a, b, c) (x, y, z) = sqrt $ fromIntegral (square (a - x) + square (b - y) + square (c - z))
    square x = x * x

    ids = map (uncurry (map . (,))) . mapMaybe uncons . tails $ [0 ..]

junctionParser :: Parser [(Int, Int, Int)]
junctionParser = ((,,) <$> Lex.decimal <* char ',' <*> Lex.decimal <* char ',' <*> Lex.decimal) `sepEndBy1` newline

data UnionFind s = UnionFind {ids :: STUArray s Int Int, sizes :: STUArray s Int Int}

initUnionFind :: Int -> ST s (UnionFind s)
initUnionFind n = liftM2 UnionFind (newListArray (0, n - 1) [0 .. n - 1]) (newArray (0, n - 1) 1)

rootSizes :: UnionFind s -> ST s [Maybe Int]
rootSizes uf = do
    idElems <- getElems (ids uf)
    roots <- mapM (root uf) idElems
    let isRoot = zipWith (==) roots [0 ..]
    sizes <- mapM (readArray (sizes uf)) idElems

    let rootSizes = zipWith (curry (fmap snd . takeIf fst)) isRoot sizes
    return rootSizes

root :: UnionFind s -> Int -> ST s Int
root uf@(UnionFind ids _) i = do
    id <- readArray ids i
    if id == i
        then return i
        else do
            groupId <- readArray ids id
            writeArray ids i groupId
            root uf id

union' uf p q = do
    union uf p q
    return uf

union :: UnionFind s -> Int -> Int -> ST s Int
union uf p q = do
    i <- root uf p
    j <- root uf q
    sizeI <- readArray (sizes uf) i
    sizeJ <- readArray (sizes uf) j
    if i == j
        then return sizeI
        else do
            if sizeI < sizeJ
                then do
                    writeArray (ids uf) i j
                    writeArray (sizes uf) j (sizeI + sizeJ)
                else do
                    writeArray (ids uf) j i
                    writeArray (sizes uf) i (sizeI + sizeJ)
            return (sizeI + sizeJ)
