import Data.List
import Data.Char

{-letterOfTheMost zips two lists-one with the number of occurrences of a symbol and the other-the symbols, finds the maximum 
"met" symbol of the string and applies alambda function which reverses the order of the first and second symbol of the tuple-}

letterOfTheMost str = (\(x,y) -> (y,x)) $ maximum $ zip (occurrences str) (removeDuplicate str)
    where
        --occurrences finds the number of occurrences of each symbol of a string and puts it (the number) to a list
        occurrences str = map length (group $ stringSorted str)
        --stringSorted concatenates a string of strings (words) to one string, then the function upperToLower is applied and finally the string is sorted
        stringSorted str = sort $ upperToLower $ concat str
        --upperToLower converts uppercase letters to lowercase
        upperToLower [] = []
        upperToLower (x : xs) = toLower x : upperToLower xs
        --removeDuplicate removes the elements in the string which appear more than once
        removeDuplicate str = nub $ stringSorted str

str1 = ["Haskell", "Javascript", "Scheme", "C++", "Clojure", "C++"]
str2 = words "The quick brown fox jumped over the lazy dog"
str3 = ["aaaaaaaaa", "bbbbbbb", "cccccccccc", "dddddddddddd"]