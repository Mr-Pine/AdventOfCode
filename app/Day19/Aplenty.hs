module Day19.Aplenty (solveDay19) where
import Util (Parser, example, parseOrError, input)
import Text.Megaparsec.Char (char, alphaNumChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec (some, sepBy, (<|>), sepEndBy, MonadParsec (try))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)

solveDay19 = do
    putStrLn "Day 19 - Aplenty:"
    input <- input 19
    (workflowList, parts) <- parseOrError inputParser input
    let workflows = workflowMap workflowList
    print . part1 workflows $ parts

part1 workflows = sum . map total . filter (processPart workflows)

data Part = Part
  { x :: Int
  , m :: Int
  , a :: Int
  , s :: Int } deriving (Show, Eq)
type Workflow = (String, [Rule])
type Rule = Part -> Maybe (Either String Bool)

processPart :: Map.Map String [Rule] -> Part -> Bool
processPart workflows = applyRule "in"
    where
        applyRule name part = case head . mapMaybe ($ part) $ rules of
                Left next -> applyRule next part
                Right result -> result
            where
                rules = workflows Map.! name

workflowMap :: [Workflow] -> Map.Map String [Rule]
workflowMap = Map.fromList

total part = x part + m part + a part + s part

inputParser :: Parser ([Workflow], [Part])
inputParser = (,) <$> (workflowsParser <* space) <*> partsParser

partsParser :: Parser [Part]
partsParser = partParser `sepEndBy` space
partParser :: Parser Part
partParser = Part <$> (string "{x=" *> L.decimal) <*> (string ",m=" *> L.decimal) <*> (string ",a=" *> L.decimal) <*> (string ",s=" *> L.decimal <* char '}')

workflowsParser :: Parser [Workflow]
workflowsParser = workflowParser `sepEndBy` space
workflowParser :: Parser Workflow
workflowParser = (,) <$> name <*> (char '{' *> (rule `sepBy` char ',') <* char '}')
    where
        name = some alphaNumChar
        rule = try conditionalRule <|> constantRule
        result = Right True <$ char 'A' <|> Right False <$ char 'R' <|> Left <$> some alphaNumChar :: Parser (Either String Bool)
        conditionalRule = buildConditionalRule <$> conditionOperand <*> operator <*> L.decimal <*> (char ':' *> result)
        buildConditionalRule :: (Part -> Int) -> (Int -> Int -> Bool) -> Int -> Either String Bool -> (Part -> Maybe (Either String Bool))
        buildConditionalRule lOperandTransformer comparator rOperand resultValue part = if condition part then Just resultValue else Nothing
            where
                condition = flip comparator rOperand . lOperandTransformer
        operator = (<) <$ char '<' <|> (>) <$ char '>' :: Parser (Int -> Int -> Bool)
        conditionOperand = x <$ char 'x' <|> m <$ char 'm' <|> a <$ char 'a' <|> s <$ char 's' :: Parser (Part -> Int)

        constantRule = const . Just <$> result