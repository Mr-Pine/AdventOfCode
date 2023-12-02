module Util where
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec (Parsec)
input i = readFile ("./input/" ++ show i ++ ".input")
example i = readFile ("./input/" ++ show i ++ ".example")
type Parser = Parsec Void Text