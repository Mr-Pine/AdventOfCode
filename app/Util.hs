{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Util where
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec (Parsec, parse, errorBundlePretty)
import GHC.RTS.Flags (RTSFlags(debugFlags))
import Debug.Trace (trace)
import Debug.Pretty.Simple (pTrace, pTraceWith)

input i = readFile ("./input/" ++ show i ++ ".input")
example i = readFile ("./input/" ++ show i ++ ".example")

type Parser = Parsec Void String

parseOrError :: Parser a -> String -> IO a
parseOrError parser input = case parse parser "" input of
    Left err ->  do
        putStrLn $ errorBundlePretty err
        error "Parsing failed :("
    Right value -> pure value

debug :: Prettify a => a -> a
debug = debugMessage ""
debugMessage :: Prettify a => String -> a -> a
debugMessage = debugMessageWith prettify

debugPlain :: Show a => a -> a
debugPlain = debugMessagePlain ""
debugMessagePlain :: Show a => String -> a -> a
debugMessagePlain = debugMessageWith show


debugMessageWith :: (a -> [Char]) -> [Char] -> a -> a
debugMessageWith a s = pTraceWith ((s ++) . a)

class Prettify a where
  prettify :: a -> String

instance Prettify a => Prettify [a] where
    prettify = show . map prettify