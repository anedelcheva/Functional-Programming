count :: Int -> [Int] -> Int
count x list = iter list 0
    where
        iter [] result = result
        iter list result
            | x == head list = iter (tail list) (result + 1)
            | otherwise = iter (tail list) result

occurrences :: [Int] -> [Int] -> [Int]
occurrences l1 l2 = (reverse (iter l1 l2 []))
    where
        iter [] l2 result = result
        iter l1 l2 result = iter (tail l1) l2 ((count (head l1) l2) : result)