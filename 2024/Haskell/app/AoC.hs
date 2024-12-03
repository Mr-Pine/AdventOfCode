import Advent (AoCOpts (AoCOpts), AoCUserAgent (AoCUserAgent), defaultAoCOpts)
import Data.List (intersperse)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (pack)
import System.Environment (getArgs)
import Util (example, input)

import Day1.HistorianHysteria (solveDay1)
import Day2.RedNosedReports (solveDay2)
import Day3.MullItOver (solveDay3)


main = do
    argStrings <- getArgs
    let isExample = ((Just "example" ==) . listToMaybe) argStrings
    let strippedArgsStrings = if isExample then tail argStrings else argStrings
    let requestedDays = daysOrAll $ map read strippedArgsStrings
    let days = filter ((`elem` requestedDays) . fst) numberedDays
    opts <- aocOpts
    let inputSupplier = (if isExample then example else input) opts
    let daysWithArgument = map (uncurry $ supplyWithArgument inputSupplier) days
    exec daysWithArgument

exec (x : xs) = do
    x
    exec xs
exec [] = putStrLn "\nHappy Coding!"

numberedDays = zip [1 ..] days

supplyWithArgument inputSupplier day solver = do
    input <- inputSupplier day
    solver input

daysOrAll [] = [1 ..]
daysOrAll x = x

days :: [String -> IO ()]
days = [solveDay1, solveDay2, solveDay3]

empty :: Bool -> IO ()
empty _ = do
    print "Nothing for this day :("

aocOpts = do
    sessionKey <- readFile ".sessionKey"
    let userAgent = AoCUserAgent (pack "https://github.com/Mr-Pine/AdventOfCode/tree/master/2024/Haskell") (pack "git@mr-pine.de using https://hackage.haskell.org/package/advent-of-code-api")
    return $ defaultAoCOpts userAgent 2024 sessionKey
