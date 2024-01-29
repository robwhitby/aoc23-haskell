import Data.Char (isDigit, digitToInt)

main = do
    example <- readFile "inputs/day01ex.txt"
    example2 <- readFile "inputs/day01ex2.txt"
    input <- readFile "inputs/day01.txt"
    print $ part1 $ lines example
    print $ part1 $ lines input
    print $ part1 $ map replaceNumbers $ lines example2
    print $ part1 $ map replaceNumbers $ lines input

part1 =
  sum . map ((\s -> read [head s, last s]) . filter isDigit)

replaceNumbers :: String -> String
replaceNumbers "" = ""
replaceNumbers ('o':'n':'e':t) = "one1one" ++ replaceNumbers t
replaceNumbers ('t':'w':'o':t) = "two2two" ++ replaceNumbers t
replaceNumbers ('t':'h':'r':'e':'e':t) = "three3three" ++ replaceNumbers t
replaceNumbers ('f':'o':'u':'r':t) = "four4four" ++ replaceNumbers t
replaceNumbers ('f':'i':'v':'e':t) = "five5five" ++ replaceNumbers t
replaceNumbers ('s':'i':'x':t) = "six6six" ++ replaceNumbers t
replaceNumbers ('s':'e':'v':'e':'n':t) = "seven7seven" ++ replaceNumbers t
replaceNumbers ('e':'i':'g':'h':'t':t) = "eight8eight" ++ replaceNumbers t
replaceNumbers ('n':'i':'n':'e':t) = "nine9nine" ++ replaceNumbers t
replaceNumbers (h:t) = h : replaceNumbers t
