module Util where
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec (Parsec, parse, errorBundlePretty)
input i = readFile ("./input/" ++ show i ++ ".input")
example i = readFile ("./input/" ++ show i ++ ".example")

type Parser = Parsec Void String

parseOrError :: Parser a -> String -> IO a
parseOrError parser input = case parse parser "" input of
    Left err ->  do
        putStrLn $ errorBundlePretty err
        error "Parsing failed :("
    Right value -> pure value