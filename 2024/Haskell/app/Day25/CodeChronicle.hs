module Day25.CodeChronicle (solveDay25) where

import Util
import Text.Megaparsec (count, sepEndBy1)
import Control.Applicative (some, (<|>))
import Text.Megaparsec.Char (char, newline, space)
import Data.List (transpose)
import Control.Monad (liftM2)

solveDay25 input _ = do
    putStrLn "Day 25 - Code Chronicle"
    items <- parseOrError parser input
    print $ part1 items

maxHeight = 5

part1 items = length . filter id $ liftM2 mayFit keys locks
    where
        keys = filter isKey items
        locks = filter (not . isKey) items

data Item = Key [Int] | Lock [Int] deriving (Show,Eq)
isKey (Key _) = True
isKey _ = False

mayFit (Key keyPins) (Lock lockPins) = all (<= maxHeight) $ zipWith (+) keyPins lockPins

parser :: Parser [Item]
parser = itemParser `sepEndBy1` space
    where
        itemParser = keyParser <|> lockParser
        keyParser = Key <$> (some (char '.') *> newline *> contentParser <* some (char '#'))
        lockParser = Lock <$> (some (char '#') *> newline *> contentParser <* some (char '.'))
        contentParser = map (length . filter (=='#')) . transpose <$> count maxHeight (some (char '#' <|> char '.') <* newline)
