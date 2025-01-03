import Advent (AoCOpts (AoCOpts), AoCUserAgent (AoCUserAgent), defaultAoCOpts)
import Data.List (intersperse)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (pack)
import System.Environment (getArgs)
import Util (example, input)

import Day1.HistorianHysteria (solveDay1)
import Day2.RedNosedReports (solveDay2)
import Day3.MullItOver (solveDay3)
import Day4.CeresSearch (solveDay4)
import Day5.PrintQueue (solveDay5)
import Day6.GuardGallivant (solveDay6)
import Day7.BridgeRepair (solveDay7)
import Day8.ResonantCollinearity (solveDay8)
import Day9.DiskFragmenter (solveDay9)
import Day10.HoofIt (solveDay10)
import Day11.PlutonianPebbles (solveDay11)
import Day12.GardenGroups (solveDay12)
import Day13.ClawContraption (solveDay13)
import Day14.RestroomRedoubt (solveDay14)
import Day15.WarehouseWoes (solveDay15)
import Day16.ReindeerMaze (solveDay16)
import Day17.ChronospatialComputer (solveDay17)
import Day18.RamRun (solveDay18)
import Day19.LinenLayout (solveDay19)
import Day20.RaceCondition (solveDay20)
import Day21.KeypadConundrum (solveDay21)
import Day22.MonkeyMarket (solveDay22)
import Day23.LanParty (solveDay23)
import Day24.CrossedWires (solveDay24)
import Day25.CodeChronicle (solveDay25)


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
days = [solveDay1, solveDay2, solveDay3, solveDay4, solveDay5, solveDay6, solveDay7, solveDay8, solveDay9, solveDay10, solveDay11, solveDay12, solveDay13, solveDay14, solveDay15, solveDay16, solveDay17, solveDay18, solveDay19, solveDay20, solveDay21, solveDay22, solveDay23, solveDay24, solveDay25]

empty :: Bool -> IO ()
empty _ = do
    print "Nothing for this day :("

aocOpts = do
    sessionKey <- readFile ".sessionKey"
    let userAgent = AoCUserAgent (pack "https://github.com/Mr-Pine/AdventOfCode/tree/master/2024/Haskell") (pack "git@mr-pine.de using https://hackage.haskell.org/package/advent-of-code-api")
    return $ defaultAoCOpts userAgent 2024 sessionKey
