import Day1.Trebuchet (solveDay1)
import Day2.CubeConundrum (solveDay2)
import Day3.GearRatios (solveDay3)
import System.Environment (getArgs)
import Data.List (intersperse)

main = do
    argStrings <- getArgs
    let args = map read argStrings :: [Int]
    let days = (intersperse (putStrLn "") . map snd . filter ((`elem` args) . fst)) numberedDays
    exec days

exec (x:xs) = do
    x
    exec xs
exec [] = putStrLn "\nHappy Coding!"
    
numberedDays = zip [1..] days

days :: [IO ()]
days = [solveDay1, solveDay2, solveDay3]