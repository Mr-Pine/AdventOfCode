{-# LANGUAGE NamedFieldPuns #-}
module Day24.CrossedWires (solveDay24) where

import Util
import Text.Megaparsec (someTill, sepEndBy1, some)
import Text.Megaparsec.Char (alphaNumChar, char, space, newline, string)
import Data.Bits ((.&.), xor, (.|.))
import Control.Applicative ((<|>))
import Prelude hiding (id)
import Debug.Trace (trace)
import Data.Maybe (fromMaybe, fromJust)
import Data.Array ((//), (!), array, Array, listArray)
import Data.List (nub, sort, sortBy, find)
import Prelude hiding (id)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, filterWithKey)
import GHC.Utils.Misc (sortWith)

solveDay24 input _ = do
    putStrLn "Day 24 - Crossed Wires"
    values <- parseOrError valueParser input
    print $ part1 values

part1 values = getZValue valueArray idxMap
    where
        valueArray = buildValueArray (map (mapId (idxMap Map.!)) values)
        idxMap = idMap values

ids :: (Ord a, Eq a) => [WireValue a] -> [a]
ids = nub . sort . map getId

idMap :: [WireValue String] -> Map String Int
idMap = Map.fromList . flip zip [0..] . ids

data WireValue a = Initial {id :: a, value :: Int} | Gate {id :: a, op :: Int -> Int -> Int, inputs :: (a, a)}

instance Show a => Show (WireValue a) where
    show (Initial {id, value}) = show id ++ ": " ++ show value
    show (Gate {id,inputs}) = show inputs ++ " -> " ++ show id

getId (Initial{id}) = id
getId (Gate{id}) = id

mapId t (Initial {id, value}) = Initial (t id) value
mapId t (Gate {id, op, inputs}) = Gate (t id) op (tmap t inputs)

getZValue arr = Map.foldr ((. (*2)) . (+)) 0 . Map.map (arr !) . filterWithKey (const . (=='z') . head)

buildValueArray vals = arr
    where
        arr = listArray (0,length vals - 1) (map (evalValue . findValForId) [0..length vals - 1])
        findValForId target = fromJust $ find ((==target) . getId) vals
        evalValue (Initial{value}) = value
        evalValue (Gate {op, inputs}) = uncurry op . tmap (arr !) $ inputs

valueParser :: Parser [WireValue String]
valueParser = (++) <$> initialValues <* newline <*> gates
    where
        initialValueParser = Initial <$> someTill alphaNumChar (char ':') <* space <*> number
        initialValues = initialValueParser `sepEndBy1` newline

        gates = gateParser `sepEndBy1` newline
        gateParser = toGate <$> some alphaNumChar <* space <*> operationParser <* space <*> some alphaNumChar <* string " -> " <*> some alphaNumChar
        operationParser :: Parser (Int -> Int -> Int)
        operationParser = (.&.) <$ string "AND" <|> xor <$ string "XOR" <|> (.|.) <$ string "OR"
        toGate id1 op id2 target = Gate target op (id1,id2)
