{-# LANGUAGE NamedFieldPuns #-}

module Day24.CrossedWires (solveDay24) where

import Control.Applicative ((<|>))
import Data.Array (Array, array, listArray, (!), (//))
import Data.Bits (xor, (.&.), (.|.))
import Data.List (find, nub, sort, sortBy, intercalate)
import Data.List.HT (padLeft)
import Data.Map.Strict (Map, filterWithKey, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Tuple (swap)
import Debug.Trace (trace)
import GHC.Utils.Misc (sortWith)
import Text.Megaparsec (sepEndBy1, some, someTill)
import Text.Megaparsec.Char (alphaNumChar, char, newline, space, string)
import Util
import Prelude hiding (id)

solveDay24 input _ = do
    putStrLn "Day 24 - Crossed Wires"
    values <- parseOrError valueParser input
    print $ part1 values
    print $ part2 values

part1 values = getZValue valueArray idxMap
  where
    valueArray = buildValueArray (map (mapId (idxMap Map.!)) values)
    idxMap = idMap values

{-
 -      x01 x00
 -      y01 y00
 -     ----
 -      a01
 - c01  c00
 ---------------
 -      z01 z00
 -
 - x00 ^ y00 = z01 baseXor
 - x01 ^ y01 = a01 baseXor (tmpSum)
 -
 - x00 & y00 = c00 baseAnd (carry)
 - x01 & y01 = b01 baseAnd (tmpCarry)
 -
 - a01 ^ c00 = z01 carryXor
 - a01 & c00 = d01 carryAnd (tmpCarry2)
 -
 - b01 | d01 = c01 carryOr (carry)
 -
 - z_n = a_n ^ c_{n-1} = (x_n ^ y_n) ^ (b_{n-1} | d_{n-1})
 -
 -}
part2 values = intercalate "," . nub . sort . map getId $ faultyXors ++ faultyLastSum ++ singlePartialSums ++ singlePartialCarries
  where
    faultyXors = filter ((['x', 'y'] /=) . sort . map head . tupleToList . inputs) . filter ((/= 'z') . head . getId) . filter ((== XOR) . op) . filter (not . isInitial) $ values
    faultyLastSum = filter ((/= XOR) . op) . filter ((/= "z45") . getId) . filter ((== 'z') . head . getId) . filter (not . isInitial) $ values
    singlePartialSums = filter (not . hasFullSum . getId) . map fromJust . takeWhile isJust . map (flip find values . isPartialSum . tupleToList) . drop 1 $ zip xIds yIds
    singlePartialCarries = filter (not . hasFullCarry . getId) . map fromJust . takeWhile isJust . map (flip find values . isPartialCarry . tupleToList) . drop 1 $ zip xIds yIds

    isPartialSum inp (Gate {op,inputs}) = op == XOR && inp == (sort . tupleToList $ inputs)
    isPartialSum _ _ = False
    hasFullSum id = any ((id `elem`) . tupleToList . inputs) . filter ((==XOR) . op) . filter (not . isInitial) $ values

    isPartialCarry inp (Gate {op,inputs}) = op == AND && inp == (sort . tupleToList $ inputs)
    isPartialCarry _ _ = False
    hasFullCarry id = any ((id `elem`) . tupleToList . inputs) . filter ((==OR) . op) . filter (not . isInitial) $ values

    xIds = map (('x' :) . padLeft '0' 2 . show) [0 ..]
    yIds = map (('y' :) . padLeft '0' 2 . show) [0 ..]

data Gate a = HalfAdder {in1 :: a, in2 :: a, sum :: a, carry :: a} | FullAdder {in1 :: a, in2 :: a, carryIn :: a, halfSum :: a, sum :: a, tmpCarry1 :: a, tmpCarry2 :: a, carryOut :: a} deriving (Show, Eq)

findHalfAdder gates in1 in2 = HalfAdder in1 in2 . getId <$> find (matchGate XOR (in1, in2)) gates <*> (getId <$> find (matchGate AND (in1, in2)) gates)

dependencies values tid = getDependencies <$> setter
  where
    getDependencies setter = if isInitial setter then [tid] else [show (op setter), fst (inputs setter), snd (inputs setter)]
    setter = find ((== tid) . getId) values

ids :: (Ord a, Eq a) => [WireValue a] -> [a]
ids = nub . sort . map getId

idMap :: [WireValue String] -> Map String Int
idMap = Map.fromList . flip zip [0 ..] . ids

data Operation = AND | XOR | OR deriving (Show, Eq, Ord)
apply AND = (.&.)
apply XOR = xor
apply OR = (.|.)

data WireValue a = Initial {id :: a, value :: Int} | Gate {id :: a, op :: Operation, inputs :: (a, a)} deriving (Show, Eq, Ord)
isInitial (Initial{}) = True
isInitial _ = False

matchGate op inputs (Gate{op = gateOp, inputs = gateInputs}) = op == gateOp && (inputs == gateInputs || inputs == swap gateInputs)
matchGate _ _ _ = False

getId (Initial{id}) = id
getId (Gate{id}) = id

mapId t (Initial{id, value}) = Initial (t id) value
mapId t (Gate{id, op, inputs}) = Gate (t id) op (tmap t inputs)

getZValue arr = Map.foldr ((. (* 2)) . (+)) 0 . Map.map (arr !) . filterWithKey (const . (== 'z') . head)

buildValueArray vals = arr
  where
    arr = listArray (0, length vals - 1) (map (evalValue . findValForId) [0 .. length vals - 1])
    findValForId target = fromJust $ find ((== target) . getId) vals
    evalValue (Initial{value}) = value
    evalValue (Gate{op, inputs}) = uncurry (apply op) . tmap (arr !) $ inputs

valueParser :: Parser [WireValue String]
valueParser = (++) <$> initialValues <* newline <*> gates
  where
    initialValueParser = Initial <$> someTill alphaNumChar (char ':') <* space <*> number
    initialValues = initialValueParser `sepEndBy1` newline

    gates = gateParser `sepEndBy1` newline
    gateParser = toGate <$> some alphaNumChar <* space <*> operationParser <* space <*> some alphaNumChar <* string " -> " <*> some alphaNumChar
    operationParser = AND <$ string "AND" <|> XOR <$ string "XOR" <|> OR <$ string "OR"
    toGate id1 op id2 target = Gate target op (id1, id2)
