primes :: [Int]
primes = filter isPrime [1..]
    where
        divisors n = [x | x <- [1..n], mod n x == 0]
        sumOfDivisors n = sum $ divisors n
        isPrime n = (n + 1) == sumOfDivisors n

squarePrimes :: [Int]
squarePrimes = map (^2) $ filter isPrime [1..]
    where
        divisors n = [x | x <- [1..n], mod n x == 0]
        sumOfDivisors n = sum $ divisors n
        isPrime n = (n + 1) == sumOfDivisors n

sumStreams :: [Int] -> [Int] -> [Int]
sumStreams s1 s2 = map (\(x,y) -> x+y) (zip s1 s2)

alternateStreams :: [Int] -> [Int] -> [Int]
alternateStreams s1 s2 = concat $ map (\(x,y) -> [x,y]) $ zip s1 s2