{-# LANGUAGE TupleSections #-}

module Day15.WarehouseWoes (solveDay15) where

import Control.Applicative ((<|>))
import Data.Foldable (foldrM)
import Data.List (find, intercalate, nub)
import Data.Map (Map, assocs, delete, fromList, insert, mapKeys, (!?))
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing, listToMaybe)
import Data.Tuple.HT (mapSnd)
import GHC.Data.Maybe (isJust)
import Text.Megaparsec (many, sepEndBy1, some)
import Text.Megaparsec.Char (char, newline)
import Util
import Prelude hiding (Left, Right)

solveDay15 input _ = do
    putStrLn "Day 15 - Warehouse Woes:"
    (warehouse, directions) <- parseOrError parser input
    print . part1 warehouse $ directions
    print . part2 warehouse $ directions

displayWarehouse (width, height) warehouse robot = intercalate "\n" . map (map toChar) $ positions
  where
    positions = map (map fst) . xyEnumerate . replicate height . replicate width $ ()

    toChar p
        | p == robot = display Robot
        | otherwise = display (fromMaybe Free (warehouse !? p))

part1 warehouse directions = score . fst . uncurry (moveRobot directions) . transformMap . map (map transformBox) . xyEnumerate $ warehouse
part2 warehouse directions = score . fst . uncurry (moveRobot directions) . transformMap . map (concatMap (expand . transformBox)) . xyEnumerate $ warehouse
  where
    expand ((x, y), Robot) = [((2 * x, y), Robot), ((2 * x + 1, y), Free)]
    expand ((x, y), Box _) = [((2 * x, y), Box [(2 * x, y), (2 * x + 1, y)]), ((2 * x + 1, y), Box [(2 * x, y), (2 * x + 1, y)])]
    expand ((x, y), a) = [((2 * x, y), a), ((2 * x + 1, y), a)]

score = sum . map (gps . head) . nub . map (getBoxPos . snd) . filter (isBox . snd) . assocs

transformBox (p, Box') = (p, Box [p])
transformBox a = a

data MapTile = Wall | Box [(Int, Int)] | Box' | Robot | Free deriving (Eq, Show)
isBox (Box _) = True
isBox _ = False

getBoxPos (Box ps) = ps

freeRobot Robot = Free
freeRobot x = x

display Free = '.'
display Robot = '@'
display Wall = '#'
display (Box [x]) = 'O'
display (Box [a, b]) = 'G'

moveRobot ds warehouse pos = foldl (flip (uncurry . moveRobotStep)) (warehouse, pos) ds

moveRobotStep dir warehouse pos = maybe (warehouse, pos) (,targetPosition) movedBox
  where
    targetPosition = moveInDirection dir pos
    movedBox = moveBox warehouse dir (warehouse !? targetPosition)

moveBox warehouse dir Nothing = Just warehouse
moveBox warehouse dir (Just Wall) = Nothing
moveBox warehouse dir (Just (Box ps)) = flip (foldr (`insert` Box targetPositions)) targetPositions . flip (foldr delete) ps <$> movedWarehouse
  where
    targetPositions = map (moveInDirection dir) ps
    checkPositions = filter (not . (`elem` ps)) targetPositions
    movedWarehouse = foldrM (\pos warehouse -> moveBox warehouse dir (warehouse !? pos)) warehouse checkPositions

gps (x, y) = 100 * y + x

transformMap warehouse = (warehouseMap, robotPosition)
  where
    robotPosition = fst . fromJust . find ((== Robot) . snd) . concat $ warehouse

    robotRemoved = map (mapSnd freeRobot) . concat $ warehouse
    warehouseMap = fromList . filter ((/= Free) . snd) $ robotRemoved

parser = (,) <$> mapParser <* newline <*> directionsParser

mapParser :: Parser [[MapTile]]
mapParser = lineParser `sepEndBy1` newline
  where
    lineParser = some tileParser
    tileParser = Wall <$ char '#' <|> Box' <$ char 'O' <|> Robot <$ char '@' <|> Free <$ char '.'

directionsParser :: Parser [Direction]
directionsParser = stepParser `sepEndBy1` many newline
  where
    stepParser = Up <$ char '^' <|> Down <$ char 'v' <|> Left <$ char '<' <|> Right <$ char '>'
