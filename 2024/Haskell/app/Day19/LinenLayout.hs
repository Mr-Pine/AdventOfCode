{-# LANGUAGE TupleSections #-}
module Day19.LinenLayout (solveDay19) where

import Util
import Text.Megaparsec (some, sepEndBy1)
import Text.Megaparsec.Char (char, space, hspace, newline)
import Control.Applicative ((<|>))
import Debug.Trace (trace)
import Data.Map (Map, member, (!?), empty, insert, (!))
import GHC.Data.Maybe (firstJusts, fromMaybe)
import Data.Maybe (isJust)
import Control.Monad (join)

solveDay19 input _ = do
    putStrLn "Day 19 - Linen Layout:"
    hotel@(patterns, designs) <- parseOrError hotelParser input
    print $ uncurry part1 hotel
    print $ uncurry part2 hotel

part1 = ((length . filter (/=0)) . ) . mapConstructions empty
part2 = (sum . ) . mapConstructions empty

mapConstructions _ _ [] = []
mapConstructions memo patterns (d:designs) = res : mapConstructions memo patterns designs
    where
        (newMemo,res) = constructDesign memo patterns d

data Color = White | Blue | Black | Red | Green deriving (Show,Eq,Ord)
type Pattern = [Color]

starts pat = (==pat) . take (length pat)
splitPattern pat = (pat,) . drop (length pat)

constructDesign :: Map Pattern Int -> [Pattern] -> Pattern -> (Map Pattern Int, Int)
constructDesign memo _ [] = (memo,1)
constructDesign memo patterns design
    | design `member` memo = (memo, memo ! design)
    | otherwise = findPatterns memo patterns design
    where

        findPatterns :: Map Pattern Int -> [Pattern] -> Pattern -> (Map Pattern Int, Int)
        findPatterns memo [] design = fillMemo design (memo,0)
        findPatterns memo (p:ps) design
            | not (p `starts` design) = findPatterns memo ps design
            | otherwise = fillMemo design $ combineResults res (findPatterns newMemo ps design)
            where
                res@(newMemo,_) = constructDesign memo patterns (drop (length p) design)
                combineResults (_,options) (memo, options2) = (memo, options + options2)

        fillMemo design (memo, res) = (insert design res memo, res)

patternParser :: Parser Pattern
patternParser = some colorParser
    where
        colorParser = White <$ char 'w' <|> Blue <$ char 'u' <|> Black <$ char 'b' <|> Red <$ char 'r' <|> Green <$ char 'g'

hotelParser = (,) <$> (patternParser `sepEndBy1` (char ',' <* hspace)) <* space <*> patternParser `sepEndBy1` newline
