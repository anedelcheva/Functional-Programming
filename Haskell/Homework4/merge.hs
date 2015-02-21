merge [] l = l
merge l [] = l
merge l1 l2 = concat $ map (\(x,y) -> [x,y]) (zip l1 l2)

l1 = [1,2,3]
l2 = [4,5,6]