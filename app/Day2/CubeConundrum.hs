module Day2.CubeConundrum where
import Util (example, Parser)

solveDay2 = do
    putStrLn "Day 2 - Cube Conundrum:"
    input <- example 2
    let games = parseGames input
    print (show games)

data Subset = Subset {
    red :: Int,
    green :: Int,
    blue :: Int
} deriving Show

data Game = Game {
    id :: Int,
    subsets :: [Subset]
} deriving Show

parseGames :: String -> [Game]
parseGames s = []
