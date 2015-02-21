import Data.List

step :: Int -> [Int] -> Bool
step n list = sum list <= n

sortDescending :: [Int] -> [Int]
sortDescending = reverse.sort

balance :: Int -> [Int] -> Int
balance n numbers = helper n sorted
    where
        sorted = sortDescending numbers
        helper _ [] = 0
        helper limit list@(x : xs)
            | step n list = 0
            | otherwise = 1 + helper n xs