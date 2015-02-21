listToNumber :: [Int] -> Int
listToNumber items = iter items 0
	where
		iter [] result = result
		iter (x : xs) result = iter xs (x + result*10)