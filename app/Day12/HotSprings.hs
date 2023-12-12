{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Day12.HotSprings (solveDay12) where
import Util (Parser, example, parseOrError, input, debugMessagePlain)
import Text.Megaparsec.Char (char, newline, space)
import Text.Megaparsec ((<|>), many, sepBy, sepEndBy, endBy)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List (intercalate)
import GHC.Generics (Generic)
import Cantor (Cantor (fromCantor, toCantor, cardinality), cantorEnumeration, Cardinality (Finite))
import Data.Chimera (memoizeFix)

solveDay12 = do
    putStrLn "Day 12 - Hot Springs:"
    input <- example 12
    records <- parseOrError recordsParser input
    print "hi"
    print . part1 $ records
    let steps = cantorEnumeration :: [Step]
    print . take 100 $ steps
    let three = [1, 2,3, 4, 5] :: [Int]
    print . fromCantor $ three
    print . fromCantor . map BoundedInt $ three
    --print  . fromCantor $ Step True [Unknown, Damaged, Unknown, Damaged, Unknown, Damaged, Unknown, Damaged, Unknown, Damaged, Unknown, Damaged, Unknown, Damaged, Unknown] (map BoundedInt [1,3,1,6])
    --print . part2 $ records

data Spring = Operational | Damaged | Unknown deriving (Show, Eq, Generic, Cantor)
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
    
data Step = Step Bool [Spring] [BoundedInt] deriving (Show, Eq, Generic, Cantor)
newtype BoundedInt = BoundedInt Int deriving (Show, Eq, Generic)
instance Cantor BoundedInt where
    toCantor = BoundedInt . fromInteger
    fromCantor (BoundedInt n)= fromCantor . toInteger $ n
    cardinality = Finite 15

countOptions :: Record -> Int
countOptions (Record springs groups) = countRemainingOptions (Step False springs (map BoundedInt groups ++ [BoundedInt 0]))
    where
        -- countRemainingOptions = countRemainingOptionsF countRemainingOptions . toCantor . fromCantor
        countRemainingOptions = memoizeFix countRemainingOptionsF' . fromInteger . fromCantor

        countRemainingOptionsF' f step = countRemainingOptionsF (f . fromInteger . fromCantor) ((toCantor . toInteger) step)

        --countRemainingOptions :: inGroup: Bool -> springs:[Spring] -> groups:[Int]
        countRemainingOptionsF :: (Step -> Int) -> Step -> Int
        countRemainingOptionsF f (Step False (Operational:xs) gs) = f (Step False xs gs)
        countRemainingOptionsF f (Step True (Operational:xs) (BoundedInt 0:gs)) = f (Step False xs gs)
        countRemainingOptionsF f (Step True (Operational:xs) (_:gs)) = 0

        countRemainingOptionsF f (Step False (Damaged:xs) (BoundedInt g:gs)) = f (Step True xs (BoundedInt (g-1) : gs))
        countRemainingOptionsF f (Step True (Damaged:xs) (BoundedInt 0:gs)) = 0
        countRemainingOptionsF f (Step True (Damaged:xs) (BoundedInt g:gs)) = f (Step True xs (BoundedInt (g-1):gs))

        countRemainingOptionsF f (Step ig (Unknown:xs) gs) = f (Step ig (Operational:xs) gs) + f (Step ig (Damaged:xs) gs)

        countRemainingOptionsF f (Step ig [] z) 
            | all (==BoundedInt 0) z = 1
            | otherwise = 0

        countRemainingOptionsF f (Step x y z) = debugMessagePlain ("x: " ++ show x ++ " y: " ++ show y ++ " z: " ++ show z) 0

unfold (Record springs groups) = Record (intercalate [Unknown] . replicate 5 $ springs) (concat . replicate 5 $ groups)