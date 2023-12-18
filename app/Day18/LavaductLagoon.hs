module Day18.LavaductLagoon (solveDay18) where
import Util (Parser, example, parseOrError, input, debugMessagePlain)
import Text.Megaparsec.Char (digitChar, space, char, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char (digitToInt)
import Text.Megaparsec ((<|>), count, sepEndBy)
import Numeric (readHex)

solveDay18 = do
    putStrLn "Day 18 - Lavaduct Lagoon:"
    input <- input 18
    plan <- parseOrError planParser input
    print . containment $ plan
    print . containment . map fixEncoding $ plan

data Direction = U | D | L | R deriving (Show, Eq)
data Edge = Edge {
    direction :: Direction
    , edgeLength :: Int
    , color ::  Int
 } deriving (Show, Eq)

containment edges = area edges + 1 + totalEdgeLength edges `div` 2

totalEdgeLength = sum . map edgeLength

area :: [Edge] -> Int
area = abs . fst . foldl fold (0,0)
    where
        fold (sum, y) e = (sum + part, newY)
            where
                (part, newY) = shoelace e y
        shoelace (Edge U n _) y = (0, y + n)
        shoelace (Edge D n _) y = (0, y - n)
        shoelace (Edge L n _) y = (-n * y, y)
        shoelace (Edge R n _) y = (n * y, y)

planParser :: Parser [Edge]
planParser = edgeParser `sepEndBy` space
edgeParser :: Parser Edge
edgeParser = Edge <$> direction <* space <*> L.decimal <* space <*> color
    where
        direction = U <$ char 'U' <|> D <$ char 'D' <|> L <$ char 'L' <|> R <$ char 'R'
        color = char '(' *> char '#' *> L.hexadecimal <* char ')'

fixEncoding :: Edge -> Edge
fixEncoding (Edge _ _ colorCode) = Edge newDirection newLength 0
    where
        newLength = colorCode `div` 16
        newDirection = directionOf . (`rem` 16) $ colorCode
        directionOf 0 = R
        directionOf 1 = D
        directionOf 2 = L
        directionOf 3 = U