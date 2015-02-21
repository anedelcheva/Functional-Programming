import Data.List

digitToList :: Int -> [Int]
digitToList n
    | n < 10 = [n]
    | otherwise = digitToList (div n 10) ++ [mod n 10]

unique :: [Int] -> [Int]
unique = nub

listToNumber :: [Int] -> Int
listToNumber = foldl glue 0
    where
        glue a b = a * 10 + b

sortAscending :: Ord a => [a] -> [a]
sortAscending = sort

sortDescending :: Ord a => [a] -> [a]
sortDescending = reverse.sort

sumDigits :: Int -> Int
sumDigits = sum.digitToList

removeZeroes :: [Int] -> [Int]
removeZeroes = filter (/=0)

mergeAndSortDigits :: Int -> Int -> Int
mergeAndSortDigits x y
    | sumDigits x <= sumDigits y = listToNumber $ sortAscending twoNumbers
    | otherwise = listToNumber $ sortDescending twoNumbers
    where
        twoNumbers = removeZeroes.unique $ (digitToList x ++ digitToList y)