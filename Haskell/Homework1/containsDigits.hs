containsDigits :: Int -> Int -> Bool
containsDigits x y
    | x == y = True
    | y < 10 = containsDigit x y
    | containsDigit x (mod y 10) = containsDigits x (div y 10)
    | otherwise = False
    where
        containsDigit :: Int -> Int -> Bool
        containsDigit n x
            | n == x = True
            | n == 0 = False
            | otherwise = mod n 10 == x || containsDigit (div n 10) x