productOfDigits :: Int -> Int
productOfDigits 0 = 1
productOfDigits n = mod n 10 * productOfDigits(div n 10)