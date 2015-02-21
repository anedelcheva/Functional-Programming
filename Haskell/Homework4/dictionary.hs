import Data.List
import Data.Char

dictionary :: String -> [(String, Int)]
dictionary str = zip (removeDuplicate str) (occurrences str)
    where
        occurrences str = map length (stringGrouping str)
        stringGrouping str = group $ sort $ words $ upperToLower str
        removeDuplicate str = nub $ sort $ words $ upperToLower str
        upperToLower [] = []
        upperToLower (x : xs) = toLower x : upperToLower xs

s1 = "The quick brown fox jumps over the lazy dog"