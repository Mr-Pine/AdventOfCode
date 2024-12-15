module Day9.DiskFragmenter (solveDay9) where

import Util
import Text.Megaparsec (some, MonadParsec (eof))
import Text.Megaparsec.Char (digitChar)
import Data.Char (digitToInt, intToDigit)
import Data.String.Utils (strip)
import Control.Applicative ((<|>))
import Data.Tuple.HT (mapSnd)
import Data.Maybe (catMaybes)

solveDay9 input _ = do
    putStrLn "Day 9 - Disk Fragmenter"
    files <- parseOrError parseDisk (strip input)
    print . part1 $ files
    print . part2 $ files

part1 = checksum . defrag . toBlocks
part2 = checksum . toBlocks . fileDefrag

data File = File {size :: Int, freeAfter :: Int, id :: Int} deriving (Show, Eq)
addFreeSpace n (File s f id) = File s (f + n) id
data Block = Free | Filled Int deriving (Show, Eq)
blockFileId (Filled id) = id
representation Free = '.'
representation (Filled id) = intToDigit id

defrag blocks = defragment (length . filter (/=Free) $ blocks) blocks (reverse blocks)
    where
        defragment 0 _ _ = []
        defragment n (Filled id:xs) rs = Filled id : defragment (n - 1) xs rs
        defragment n (Free:xs) (Filled id:rs) = Filled id : defragment (n - 1) xs rs
        defragment n (Free:xs) (Free:rs) = defragment n (Free:xs) rs

fileDefrag files = defragment files
    where
        defragment [] = []
        defragment files = if length moved == length files then defragment (fixTrailingFreeSpace moved) else defragment moved ++ [last files]
            where
                moved = move (init files) (last files)
                fixTrailingFreeSpace res = init res ++ [addFreeSpace (freeAfter movee + size movee) (last res)]
                movee = last files
        move (x@(File size freeSpace id1):xs) f@(File size2 _ id2)
            | size2 <= freeSpace = File size 0 id1 : File size2 (freeSpace - size2) id2 : xs
            | otherwise = x : move xs f
        move [] f = []

checksum = sum . map (uncurry ((. blockFileId) . (*))) . filter ((/= Free) . snd) . zip [0..]

toBlocks :: [File] -> [Block]
toBlocks = concatMap fileToBlocks
    where
        fileToBlocks (File size free id) = replicate size (Filled id) ++ replicate free Free

parseDisk :: Parser [File]
parseDisk = flip (zipWith ($)) [0..] <$> some parseFile
    where
        parseFile = File <$> (digitToInt <$> digitChar) <*> ((digitToInt <$> digitChar) <|> (0 <$ eof))
