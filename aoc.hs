import Data.Char (isDigit, readLitChar)
import Data.List
import Distribution.Simple.Utils (debug)
import Debug.Trace (trace)
main :: IO ()
main = do
    readFile "./input/1.input" >>= print . d1p1
    readFile "./input/1.input" >>= print . d1p2
    readFile "./input/1.example" >>= print . d1p2


d1p1 :: String -> Int
d1p1 = sum . map (convertLineToNumber extractNumbersSimple) . lines

d1p2 :: String -> Int
d1p2 = sum . map (convertLineToNumber hardProxy) . lines

convertLineToNumber :: (String -> String) -> String -> Int
convertLineToNumber transformer = read . (\numbers -> [head numbers, last numbers]) . transformer

extractNumbersSimple :: String -> String
extractNumbersSimple = filter isDigit

hardProxy :: String -> String
hardProxy string = extractNumbersHard string--trace ("Test: " ++ string) extractNumbersHard string

extractNumbersHard :: String -> String
extractNumbersHard ('o':'n':'e':xs) = '1' : hardProxy ("ne" ++ xs)
extractNumbersHard ('t':'w':'o':xs) = '1' : hardProxy ("wo" ++ xs)
extractNumbersHard ('t':'h':'r':'e':'e':xs) = '1' : hardProxy ("hree" ++ xs)
extractNumbersHard ('f':'o':'u':'r':xs) = '1' : hardProxy ("our" ++ xs)
extractNumbersHard ('f':'i':'v':'e':xs) = '1' : hardProxy ("ive" ++ xs)
extractNumbersHard ('s':'i':'x':xs) = '1' : hardProxy ("ix" ++ xs)
extractNumbersHard ('s':'e':'v':'e':'n':xs) = '1' : hardProxy ("even" ++ xs)
extractNumbersHard ('e':'i':'g':'h':'t':xs) = '1' : hardProxy ("ight" ++ xs)
extractNumbersHard ('n':'i':'n':'e':xs) = '1' : hardProxy ("ine" ++ xs)
extractNumbersHard (x:xs) | isDigit x = x : hardProxy xs
extractNumbersHard (x:xs) = hardProxy xs
extractNumbersHard [] = []