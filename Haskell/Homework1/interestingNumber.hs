sumOfDivisors :: Int -> Int
sumOfDivisors n = iter 1 n
    where
        iter current n
            | current > n = 0
            | mod n current == 0 = current + iter (current + 1) n
            | otherwise = iter (current + 1) n

interestingNumber :: Int -> Bool
interestingNumber n = n == sumOfDivisors2 (sumOfDivisors2 n)
    where 
        sumOfDivisors2 n = sumOfDivisors n - n