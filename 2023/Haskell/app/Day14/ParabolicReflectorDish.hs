module Day14.ParabolicReflectorDish (solveDay14) where

import Data.List (intercalate, transpose)
import Data.Sequence (Seq (Empty))
import Text.Megaparsec (sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, space)
import Util (Parser, Prettify (prettify), example, input, parseOrError, debugMessagePlain)
import qualified Data.Map.Strict as Map

solveDay14 = do
  putStrLn "Day 14 - Parabolic Reflector Dish:"
  input <- input 14
  dish <- parseOrError dishParser input
  print . part1 $ dish
  print . part2 $ dish

data Rock = Round | Sharp | None deriving (Show, Eq, Ord)

type Dish = [[Rock]]

part1 = load . tiltNorth
part2 = load . tiltCycles 1000000000

visualize :: Dish -> String
visualize = intercalate "\n" . map (concatMap prettify)

instance Prettify Rock where
  prettify Round = "O"
  prettify Sharp = "#"
  prettify None = "."

tiltNorth :: Dish -> Dish
tiltNorth = transpose . map tiltColumnNorth . transpose

tiltColumnNorth :: [Rock] -> [Rock]
tiltColumnNorth = fix rollRocksNorth
  where
    rollRocksNorth (None : Round : xs) = Round : rollRocksNorth (None : xs)
    rollRocksNorth (x : xs) = x : rollRocksNorth xs
    rollRocksNorth [] = []

    fix f a = if applied == a then a else fix f applied
      where
        applied = f a

tiltSouth = reverse . tiltNorth . reverse

tiltWest = transpose . tiltNorth . transpose

tiltEast = transpose . tiltSouth . transpose

tiltCycle =  tiltEast . tiltSouth . tiltWest . tiltNorth

tiltCycles :: Int -> Dish -> Dish
tiltCycles = tiltCycles' mempty
    where 
        tiltCycles' :: Map.Map Dish Int -> Int -> Dish -> Dish
        tiltCycles' _ 0 d = d
        tiltCycles' seen n d 
            | d `Map.member` seen = tiltCycles (n `mod` (seen Map.! d - n)) d
            | otherwise = tiltCycles' (Map.insert d n seen) (n-1) (tiltCycle d)

load = sum . zipWith beamLoad [1 ..] . reverse
  where
    beamLoad lever beam = lever * length (filter (== Round) beam)

dishParser :: Parser Dish
dishParser = rowParser `sepEndBy` space

rowParser :: Parser [Rock]
rowParser = some (Round <$ char 'O' <|> Sharp <$ char '#' <|> None <$ char '.')