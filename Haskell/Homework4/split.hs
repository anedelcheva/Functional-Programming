import Data.List

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

s = "Hello, friend! How is your Haskell?"
s1 = "How should i split on empty string?"