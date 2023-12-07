import System.Environment (getArgs)
import Data.List (intersperse)
import Day1.Trebuchet (solveDay1)
import Day2.CubeConundrum (solveDay2)
import Day3.GearRatios (solveDay3)
import Day4.Scratchcards (solveDay4)
import Day5.Fertilizer (solveDay5)
import Day6.WaitForIt (solveDay6)
import Day7.CamelCards (solveDay7)

main = do
    argStrings <- getArgs
    let args = map read argStrings :: [Int]
    let days = (intersperse (putStrLn "") . map snd . filter ((`elem` allIfEmpty args) . fst)) numberedDays
    exec days

exec (x:xs) = do
    x
    exec xs
exec [] = putStrLn "\nHappy Coding!"

numberedDays = zip [1..] days

allIfEmpty [] = [1..]
allIfEmpty x = x

days :: [IO ()]
days = [solveDay1, solveDay2, solveDay3, solveDay4, solveDay5, solveDay6, solveDay7]