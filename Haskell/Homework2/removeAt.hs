removeAt :: Int -> [a] -> [a]
removeAt index list
    | index >= length list = error "Index out of bounds"
removeAt index list = (reverse (iter index list 0 []))
    where
        iter index [] current result = result
        iter index list current result
            | index < 0 = error "Index out of bounds"
            | current == index = iter index (tail list) (current + 1) result
            | otherwise = iter index (tail list) (current + 1) (head list : result)