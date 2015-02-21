import Data.List

{-occurrences takes a string (str), groups adjacent elements into sublists if they are equal (group str);map applies the function length to the sublists 
(we get a list of the lengths of all sublists); map show converts the numbers into strings -}

{-removeTheOnes maps onto a list of strings and substitutes a "1" with ""-}

lengthEncode str = concat $ zipWith (++) (occurrences str) (group $ nub str)
    where
        occurrences str = removeTheOnes $ map show $ map length $ group str
        removeTheOnes str = map (\x -> if x == "1" then y else x) str
        y = ""