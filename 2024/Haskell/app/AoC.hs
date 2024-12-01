import System.Environment (getArgs)
import Data.List (intersperse)
import Data.Maybe (listToMaybe, fromMaybe)

main = do
    argStrings <- getArgs
    let example = ((Just "example" ==) . listToMaybe) argStrings
    let strippedArgsStrings = if example then tail argStrings else argStrings
    let args = map read strippedArgsStrings :: [Int]
    let days = (intersperse (putStrLn "") . map (($ example) . snd) . filter ((`elem` allIfEmpty args) . fst)) numberedDays
    exec days

exec (x:xs) = do
    x
    exec xs
exec [] = putStrLn "\nHappy Coding!"

numberedDays = zip [1..] days

allIfEmpty [] = [1..]
allIfEmpty x = x

days :: [Bool -> IO ()]
days = []

empty :: Bool -> IO()
empty _ = do
    print "Nothing for this day :("
