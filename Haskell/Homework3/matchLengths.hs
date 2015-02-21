--matchLengths :: [[Int]] -> [[Int]] -> Bool
matchLengths l1 l2 = areLengthsEqual $ map (\(x, y) -> abs (length x - length y)) $ zip l1 l2
    where
        areLengthsEqual list = mod (sum list) (length list) == 0

l1 = [[], [1,2], [3,4,5]]
l2 = [[1], [2,3,4], [5,6,7,8]]