module Day12.HotSprings (solveDay12) where
import Util (Parser, example, parseOrError, input, debugMessagePlain, debug)
import Text.Megaparsec.Char (char, newline, space)
import Text.Megaparsec ((<|>), many, sepBy, sepEndBy, endBy)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List (intercalate)
import Data.Array (listArray, (!))

solveDay12 = do
    putStrLn "Day 12 - Hot Springs:"
    input <- input 12
    records <- parseOrError recordsParser input
    print . part1 $ records
    print . part2 $ records

data Spring = Operational | Damaged | Unknown deriving (Show, Eq)
data Record = Record [Spring] [Int] deriving (Show, Eq)

part1 = sum . map countOptions
part2 = sum . map (countOptions . unfold)

recordsParser :: Parser [Record]
recordsParser = recordParser `endBy` newline
    where
        recordParser = Record <$> springParser <* space <*> groupParser
        spring = Operational <$ char '.' <|> Damaged <$ char '#' <|> Unknown <$ char '?'
        springParser = many spring
        groupParser = L.decimal `sepBy` char ','

countOptions :: Record -> Int
countOptions (Record springs groups) = countRemainingOptions 0 springs 0 groups
    where
        remaining = listArray ((0,0), (length springs, length groups)) (map countRemainingFromIndices indices)
        springDrops = listArray(0,length springs) (map (\n -> if n == 0 then springs else drop 1 (springDrops!(n - 1)) ) [0..length springs])
        groupDrops = listArray(0,length groups) (map (\n -> if n == 0 then groups else drop 1 (groupDrops!(n - 1)) ) [0..length groups])

        indices = concatMap ((`zip` [0..length groups]) . repeat) [0..length springs]
        countRemainingFromIndices (si, gi) = countRemainingOptions si (springDrops!si) gi (groupDrops!gi)

        --countRemainingOptions :: springIdex: Int -> springs:[Spring] -> groupIndex: Int -> groups:[Int]
        countRemainingOptions :: Int -> [Spring] -> Int -> [Int] -> Int
        countRemainingOptions si (Operational:xs) gi gs = remaining!(si+1, gi)
        countRemainingOptions si (Unknown:xs) gi gs = remaining!(si+1, gi) + getRemainingDamaged (si + 1) xs gi gs

        countRemainingOptions si x@(Damaged:xs) gi y@(g:gs) = getRemainingDamaged (si + 1) xs gi y

        countRemainingOptions si [] gi [] = 1
        countRemainingOptions si x gi y = 0

        getRemainingDamaged si remainingSprings gi y@(groupSize:gs)
            | length remainingDamaged + 1 < groupSize = 0 -- Too little springs remaining to fill group
            | not noOperational = 0 -- There are operational springs in my loop
            | noNext && null gs = 1 -- The group ends the springs, no other groups remaining
            | noNext && not (null gs) = 0 -- The group would end the springs but there are still groups remaining
            | nextNotDamaged = remaining!(si + groupSize, gi + 1) -- The element after is not damaged
            | otherwise = 0
            where
                remainingDamaged = take (groupSize - 1) remainingSprings
                noOperational = Operational `notElem` remainingDamaged -- No operational springs in group
                noNext = remainingDamaged == remainingSprings
                nextNotDamaged = length remainingSprings > length remainingDamaged && (remainingSprings !! length remainingDamaged) /= Damaged
        getRemainingDamaged _ _ _ [] = 0

unfold (Record springs groups) = Record (intercalate [Unknown] . replicate 5 $ springs) (concat . replicate 5 $ groups)