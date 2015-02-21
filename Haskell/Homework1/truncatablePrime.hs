sumOfDivisors :: Int -> Int
sumOfDivisors n = iter 1 n
    where
        iter current n
            | current > n = 0
            | mod n current == 0 = current + iter (current + 1) n
            | otherwise = iter (current + 1) n

numbersToList :: Int -> [Int]
numbersToList n
    | n == 0 = []
    | otherwise = n : numbersToList (div n 10)

isPrime :: Int -> Bool
isPrime n = sumOfDivisors n - 1 == n

truncatablePrime :: Int -> Bool
truncatablePrime n = allPrime (numbersToList n)
    where
        allPrime xs = (xs == [x | x <- xs, isPrime x])