module Day14.ParabolicReflectorDish (solveDay14) where
import Util (Parser, example, parseOrError, input, Prettify (prettify))
import Text.Megaparsec (some, (<|>), sepEndBy)
import Text.Megaparsec.Char (space, char)
import Data.List (transpose, intercalate)
import Data.Sequence (Seq(Empty))

solveDay14 = do
    putStrLn "Day 14 - Parabolic Reflector Dish:"
    input <- input 14
    dish <- parseOrError dishParser input
    --putStrLn . visualize $ dish
    --putStrLn "\n"
    --putStrLn . visualize . tiltNorth $ dish
    print . day1 $ dish
    -- print . concatMap prettify . tiltColumnNorth $ [Round, Round, None, Round, None, Round]

data Rock = Round | Sharp | None deriving (Show, Eq)
type Dish = [[Rock]]

day1 = load . tiltNorth

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
        rollRocksNorth (None:Round:xs) = Round : rollRocksNorth (None:xs)
        rollRocksNorth (x:xs) = x : rollRocksNorth xs
        rollRocksNorth [] = []

        fix f a = if applied == a then a else fix f applied
            where
                applied = f a

load = sum . zipWith beamLoad [1..] . reverse
    where
        beamLoad lever beam = lever * length (filter (==Round) beam)

dishParser :: Parser Dish
dishParser = rowParser `sepEndBy` space

rowParser :: Parser [Rock]
rowParser = some (Round <$ char 'O' <|> Sharp <$ char '#' <|> None <$ char '.')