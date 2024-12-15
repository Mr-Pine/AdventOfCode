{-# LANGUAGE TupleSections #-}

module Day6.GuardGallivant (solveDay6) where

import Control.Applicative ((<|>))
import Data.List (find)
import Data.Maybe (catMaybes, fromJust)
import Data.Set hiding (filter, map)
import qualified Data.Set as Set
import GHC.Data.Maybe (firstJusts)
import Text.Megaparsec (many, sepEndBy, some)
import Text.Megaparsec.Char (char, newline)
import Util
import Prelude hiding (Left, Right)

solveDay6 input _ = do
    putStrLn "Day 6 - Guard Gallivant:"
    state <- parseOrError stateParser input
    print . part1 $ state
    print . part2 $ state

part1 = size . Set.map fst . moves . fromJust . moveAll
part2 = size . fromList . catMaybes . progress
  where
    progress s@(State pos bounds _ moves _)
        | not (inBounds pos bounds) = []
        | not (inBounds (blockPosition s) bounds) = progress (fromJust (move s))
        | blockPosition s `member` Set.map fst moves = progress (fromJust (move s))
        | otherwise = (blockPosition s <$ (flipMaybe . moveAll . block (blockPosition s) $ s)) : progress (fromJust (move s))
    inBounds (x, y) (xLen, yLen) = y >= 0 && y < yLen && x >= 0 && x < xLen
    blockPosition = position . fromJust . move
    flipMaybe Nothing = Just ()
    flipMaybe (Just _) = Nothing
    block p (State pos bounds d ms obstacles) = State pos bounds d ms (p `insert` obstacles)

type Position = (Int, Int)

data TileType = Free | Visited | Obstacle deriving (Eq, Show)
toChar Free = '.'
toChar Obstacle = '#'
toChar Visited = 'X'

data Direction = Up | Left | Down | Right deriving (Eq, Show, Ord)
turnDirectionRight Up = Right
turnDirectionRight Right = Down
turnDirectionRight Down = Left
turnDirectionRight Left = Up

moveInDirection Up (x, y) = (x, y - 1)
moveInDirection Left (x, y) = (x - 1, y)
moveInDirection Down (x, y) = (x, y + 1)
moveInDirection Right (x, y) = (x + 1, y)

data State = State Position (Int, Int) Direction (Set (Position, Direction)) (Set Position) deriving (Eq, Show)
board (State _ _ _ _ b) = b
moves (State _ _ _ ms _) = ms
position (State p _ _ _ _) = p

move :: State -> Maybe State
move (State pos bounds d moves obstacles) = nextState
  where
    correctDirection d = if not (moveInDirection d pos `member` obstacles) then d else correctDirection (turnDirectionRight d)
    moveDirection = correctDirection d
    nextPosition = moveInDirection moveDirection pos
    currentMove = (pos, moveDirection)

    nextState = takeIf (const . not $ (currentMove `member` moves)) (State nextPosition bounds moveDirection (currentMove `insert` moves) obstacles)

moveAll s@(State (x, y) (xLen, yLen) _ _ _)
    | y >= 0 && y < yLen && x >= 0 && x < xLen = moveAll =<< move s
    | otherwise = Just s

mapLineParser :: Parser [TileType]
mapLineParser = some tileParser
  where
    tileParser = Free <$ char '.' <|> Visited <$ char '^' <|> Obstacle <$ char '#'

stateParser :: Parser State
stateParser = toState . toBoard <$> mapParser
  where
    toBoard :: [[TileType]] -> [[(Position, TileType)]]
    toBoard = zipWith (\y x -> zip (map (,y) [0 ..]) x) [0 ..]
    toObstacleSet = fromList . map fst . concatMap (filter ((== Obstacle) . snd))
    toState board = State (findGallivant board) (length (head board), length board) Up empty (toObstacleSet board)
    mapParser = mapLineParser `sepEndBy` newline

findGallivant = fst . fromJust . firstJusts . map (find ((== Visited) . snd))
