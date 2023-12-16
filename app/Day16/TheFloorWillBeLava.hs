{-# LANGUAGE OverloadedRecordDot #-}

module Day16.TheFloorWillBeLava (solveDay16) where

import Data.Array (Array, Ix (inRange), bounds, listArray, (!))
import Data.List (transpose)
import qualified Data.Map as Map
import Text.Megaparsec (many, sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, space)
import Util (Parser, example, input, parseOrError)
import qualified Data.Monoid as Map
import Data.Maybe (fromMaybe)

solveDay16 = do
  putStrLn "Day 16 - The floor will be lava:"
  input <- input 16
  grid <- parseOrError gridParser input
  -- print . raytrace $ grid
  print . part1 $ grid

part1 = Map.size . Map.filter isEnergized . raytrace

data Tile = Empty | MirrorUp | MirrorDown | SplitVertical | SplitHorizontal deriving (Show, Eq)

data Energization = Energization
  { right :: Bool,
    left :: Bool,
    up :: Bool,
    down :: Bool
  } deriving (Show, Eq)

isEnergized :: Energization -> Bool
isEnergized e = e.right || e.left || e.up || e.down

gridParser :: Parser (Array (Int, Int) Tile)
gridParser = toArray <$> gridRowParser `sepEndBy` space
  where
    toArray rows = listArray ((0, 0), (length (head rows) - 1, length rows - 1)) (concat . transpose $ rows)

gridRowParser :: Parser [Tile]
gridRowParser = some (Empty <$ char '.' <|> MirrorUp <$ char '/' <|> MirrorDown <$ char '\\' <|> SplitVertical <$ char '|' <|> SplitHorizontal <$ char '-')

type EnergyMap = Map.Map (Int, Int) Energization

data MovementDirection = R | L | U | D deriving (Show, Eq)

raytrace grid = move R (-1, 0) Map.mempty
  where
    move d c@(x,y) e = case d of
          R | inRange (bounds grid) (x + 1, y) -> goRight (x + 1, y) e
          L | inRange (bounds grid) (x - 1, y) -> goLeft (x - 1, y) e
          U | inRange (bounds grid) (x, y - 1) -> goUp (x, y - 1) e
          D | inRange (bounds grid) (x, y + 1) -> goDown (x, y + 1) e
          _ -> e

    goRight c energization
      | tileEnergization.right = newEnergization
      | otherwise = case grid ! c of
          MirrorUp -> move U c newEnergization
          MirrorDown -> move D c newEnergization
          SplitVertical -> move U c (move D c newEnergization)
          _ -> move R c newEnergization
      where
        tileEnergization = getEnergization energization c
        newEnergization = Map.insert c tileEnergization {right = True} energization

    goLeft c energization
      | tileEnergization.left = newEnergization
      | otherwise = case grid ! c of
          MirrorUp -> move D c newEnergization
          MirrorDown -> move U c newEnergization
          SplitVertical -> move U c (move D c newEnergization)
          _ -> move L c newEnergization
      where
        tileEnergization = getEnergization energization c
        newEnergization = Map.insert c tileEnergization {left = True} energization

    goUp c energization
      | tileEnergization.up = newEnergization
      | otherwise = case grid ! c of
          MirrorUp -> move R c newEnergization
          MirrorDown -> move L c newEnergization
          SplitHorizontal -> move L c (move R c newEnergization)
          _ -> move U c newEnergization
      where
        tileEnergization = getEnergization energization c
        newEnergization = Map.insert c tileEnergization {up = True} energization

    goDown c energization
      | tileEnergization.down = newEnergization
      | otherwise = case grid ! c of
          MirrorUp -> move L c newEnergization
          MirrorDown -> move R c newEnergization
          SplitHorizontal -> move R c (move L c newEnergization)
          _ -> move D c newEnergization
      where
        tileEnergization = getEnergization energization c
        newEnergization = Map.insert c tileEnergization {down = True} energization

    getEnergization energization c = fromMaybe (Energization False False False False) (Map.lookup c energization)