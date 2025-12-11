import Advent (AoCOpts (AoCOpts), AoCUserAgent (AoCUserAgent), defaultAoCOpts)
import Data.List (intersperse)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (pack)
import System.Environment (getArgs)
import Util (example, input)

import Day1.SecretEntrance (solveDay1)
import Day2.GiftShop (solveDay2)
import Day3.Lobby (solveDay3)
import Day4.PrintingDepartment (solveDay4)
import Day5.Cafeteria (solveDay5)
import Day6.TrashCompactor (solveDay6)
import Day7.Laboratories (solveDay7)
import Day8.Playground (solveDay8)
import Day9.MovieTheater (solveDay9)
import Day10.Factory (solveDay10)
import Day11.Reactor (solveDay11)

main = do
    argStrings <- getArgs
    let isExample = ((Just "example" ==) . listToMaybe) argStrings
    let strippedArgsStrings = if isExample then tail argStrings else argStrings
    let requestedDays = daysOrAll $ map read strippedArgsStrings
    let days = filter ((`elem` requestedDays) . fst) numberedDays
    opts <- aocOpts
    let inputSupplier = (if isExample then example else input) opts
    let daysWithArgument = map (uncurry $ supplyWithArgument inputSupplier isExample) days
    exec daysWithArgument

exec (x : xs) = do
    x
    putStrLn ""
    exec xs
exec [] = putStrLn "Happy Coding!"

numberedDays = zip [1 ..] days

supplyWithArgument inputSupplier isExample day solver = do
    input <- inputSupplier day
    solver input isExample

daysOrAll [] = [1 ..]
daysOrAll x = x

days :: [String -> Bool -> IO ()]
days = [solveDay1, solveDay2, solveDay3, solveDay4, solveDay5, solveDay6, solveDay7, solveDay8, solveDay9, solveDay10, solveDay11]

empty :: Bool -> IO ()
empty _ = do
    print "Nothing for this day :("

aocOpts = do
    sessionKey <- readFile ".sessionKey"
    let userAgent = AoCUserAgent (pack "https://github.com/Mr-Pine/AdventOfCode/tree/master/2025/Haskell") (pack "git@mr-pine.de using https://hackage.haskell.org/package/advent-of-code-api")
    return $ defaultAoCOpts userAgent 2025 sessionKey
