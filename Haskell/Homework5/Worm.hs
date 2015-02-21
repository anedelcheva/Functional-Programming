type Worm = (String, Int)

getLength :: Worm -> Int
getLength (_, wormLength) = wormLength

getName :: Worm -> String
getName (name, _ ) = name

nameLength :: Worm -> Int
nameLength (name, _) = length name

numberOfConsonants :: String -> Int
numberOfConsonants name = length $ consonants name
    where
        consonants [] = []
        consonants (x : xs)
            | x /= 'a' && x /= 'e' && x /= 'i' && x /= 'o' && x /= 'u' && x /= 'y' = (x : consonants xs)
            | otherwise = consonants xs

sustenance :: Worm -> Int
sustenance worm = div ((getLength worm) * (nameLength worm)) (numberOfConsonants $ getName worm)

wormsToEat :: [Worm] -> Int -> [String]
wormsToEat worms boundary = map (\worm -> getName worm) $ filter (\worm -> sustenance worm >= boundary) worms