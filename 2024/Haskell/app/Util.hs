{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module Util where

import Advent (AoC (AoCInput, AoCPrompt), Part (Part1), mkDay, mkDay_, runAoC_)
import Data.Either (fromRight)
import Data.Map ((!))
import Data.Maybe (mapMaybe)
import Data.Text (Text, unpack)
import Data.Tree (Tree (Node), foldTree)
import Data.Void (Void)
import Debug.Pretty.Simple (pTrace, pTraceWith)
import Debug.Trace (trace)
import GHC.RTS.Flags (RTSFlags (debugFlags))
import GHC.SysTools (isContainedIn)
import System.Directory (doesFileExist)
import Text.HTML.Parser (Token (ContentText, TagOpen, TagClose), parseTokens)
import Text.HTML.Tree (tokensToForest)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.List (tails)
import Data.Array (array)
import Text.Megaparsec.Char (space)
import Prelude hiding (Right, Left)
import qualified Data.Either as Either
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

input aocOpts day = do
    let filePath = "./input/" ++ show day ++ ".input"
    fileExists <- doesFileExist filePath
    if fileExists
        then
            readFile filePath
        else
            getAndSaveInput aocOpts day filePath

getAndSaveInput aocOpts day filePath = do
    let command = AoCInput (mkDay_ day)
    input <- unpack <$> runAoC_ aocOpts command
    writeFile filePath input
    putStrLn ("Got input for Day " ++ show day)
    return input

example aocOpts day = do
    let filePath = "./input/" ++ show day ++ ".example"
    fileExists <- doesFileExist filePath
    if fileExists
        then
            readFile filePath
        else
            getAndSaveExample aocOpts day filePath

getAndSaveExample aocOpts day filePath = do
    let command = AoCPrompt (mkDay_ day)
    prompt <- runAoC_ aocOpts command
    let part1Promt = prompt ! Part1
    let tokens = parseTokens part1Promt
    let filteredTokens = dropWhile (not . containsExample) tokens
    let codeTokens = findCodeBlock filteredTokens
    let text = concat [ unpack text | ContentText text <- codeTokens]

    writeFile filePath text
    putStrLn ("Got example for Day " ++ show day)
    return text

containsExample (ContentText t) = "example" `isContainedIn` unpack t
containsExample _ = False

findCodeBlock = takeWhile (not . isPreTag) . drop 1 . dropWhile (not . isPreTag)

isPreTag (TagOpen tag _) = "pre" == unpack tag
isPreTag (TagClose tag) = "pre" == unpack tag
isPreTag _ = False

type Parser = Parsec Void String

number :: Parser Int
number = Lexer.signed space Lexer.decimal

parseOrError :: Parser a -> String -> IO a
parseOrError parser input = case parse parser "" input of
    Either.Left err -> do
        putStrLn $ errorBundlePretty err
        error "Parsing failed :("
    Either.Right value -> pure value

debug :: (Prettify a) => a -> a
debug = debugMessage ""
debugMessage :: (Prettify a) => String -> a -> a
debugMessage = debugMessageWith prettify

debugPlain :: (Show a) => a -> a
debugPlain = debugMessagePlain ""
debugMessagePlain :: (Show a) => String -> a -> a
debugMessagePlain = debugMessageWith show

debugMessageWith :: (a -> [Char]) -> [Char] -> a -> a
debugMessageWith a s = pTraceWith ((s ++) . a)

class Prettify a where
    prettify :: a -> String

instance (Prettify a) => Prettify [a] where
    prettify = show . map prettify

windows n = takeWhile ((==n) . length) . map (take n) . tails

takeIf p x
    | p x = Just x
    | otherwise = Nothing

xyEnumerate :: [[a]] -> [[((Int, Int), a)]]
xyEnumerate = zipWith (zip . flip map [0..] . flip (,)) [0..]

inBounds (boundX, boundY) (x,y) = x >= 0 && x < boundX && y >= 0 && y < boundY

gridToArray grid = array ((0, 0), (subtract 1 . length . head $ xyGrid, length grid - 1)) (concat xyGrid)
  where
    xyGrid = xyEnumerate grid

data Direction = Up | Down | Left | Right deriving (Eq, Show, Ord, Enum, Generic)

directions = [Up, Down, Left, Right]

instance Hashable Direction

moveInDirection Up (x,y) = (x,y-1)
moveInDirection Down (x,y) = (x,y+1)
moveInDirection Left (x,y) = (x-1,y)
moveInDirection Right (x,y) = (x+1,y)

rotateClockwise Up = Right
rotateClockwise Down = Left
rotateClockwise Right = Down
rotateClockwise Left = Up

rotateCounterClockwise Up = Left
rotateCounterClockwise Down = Right
rotateCounterClockwise Right = Up
rotateCounterClockwise Left = Down
