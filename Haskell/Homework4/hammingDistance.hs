hammingDistance :: String -> String -> Int
hammingDistance str1 str2 = sum $ map (\(x,y) -> if x == y then 0 else 1) (zip str1 str2)

s1 = "orange"
s2 = "apples"
s3 = "karolin"
s4 = "kathrin"
s5 = "kerstin"
s6 = "1011101"
s7 = "1001001"
s8 = "2173896"
s9 = "2233796"