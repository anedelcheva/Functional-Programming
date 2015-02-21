import Data.List

columns :: [[a]] -> Int
columns m = div (sum $ map length m) (length m)

dimensions :: [[a]] -> (Int, Int)
dimensions m = (length m, columns m)

getRow :: Int -> [[a]] -> [a]
getRow row m = m !! row

getCol :: Int -> [[a]] -> [a]
getCol col m = (transpose m) !! col

diagonal :: [[a]] -> [a]
diagonal m = map (\(index, row) -> row !! index) (zip [0..(length m - 1)] m)

transpose2 :: [[a]] -> [[a]]
transpose2 m = map (\index -> getCol index m) [0..columns m - 1]

dotProduct :: (Num a) => [a] -> [a] -> a
dotProduct v1 v2 = sum $ map (\(x,y) -> x*y) (zip v1 v2)

matrixMultiply ::(Num a) => [[a]] -> [[a]] -> [[a]]
matrixMultiply m1 m2 = map (\row -> timesRow row m2) m1
    where
        timesRow row m = map (\column -> dotProduct row column) (transpose m)

m1 = [[1,2,3], [4,5,6], [7,8,9]]
m2 = [[1,2,3,4], [5,6,7,8], [9,10,11,12]]