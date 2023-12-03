module Util where
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec (Parsec, parse, errorBundlePretty)
import GHC.RTS.Flags (RTSFlags(debugFlags))
import Debug.Trace (trace)

input i = readFile ("./input/" ++ show i ++ ".input")
example i = readFile ("./input/" ++ show i ++ ".example")

type Parser = Parsec Void String

parseOrError :: Parser a -> String -> IO a
parseOrError parser input = case parse parser "" input of
    Left err ->  do
        putStrLn $ errorBundlePretty err
        error "Parsing failed :("
    Right value -> pure value

debug :: Show a => a -> a
debug = debugMessage ""
debugMessage s x = trace (s ++ show x) x 

-- debugMessageWith :: Show b => String -> (a -> b) -> a -> a
debugMessageWith s a x = do
    debugMessage s (a x)
    x