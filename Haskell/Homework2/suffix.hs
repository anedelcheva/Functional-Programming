suffix :: (Eq a) => [a] -> [a] -> Bool
suffix [] [] = True
suffix [] _ = True
suffix _ [] = False
suffix list1 list2
    | head (reverse list1) == head (reverse list2) = suffix (tail list1) (tail list2)
    | otherwise = False