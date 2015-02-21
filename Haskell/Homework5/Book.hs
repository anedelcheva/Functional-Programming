type Book = (String, Int)

getPages :: Book -> Int
getPages (_, pages) = pages

getTitle :: Book -> String
getTitle (title, _) = title

getTitleLength :: Book -> Int
getTitleLength (title, _) = length title

numberOfVowels :: String -> Int
numberOfVowels title = length $ filterVowels title
    where
        filterVowels [] = []
        filterVowels (x : xs)
            | x == 'a' || x == 'A' || x == 'e' || x == 'E' || x == 'i' || x == 'I' || x == 'o' || x == 'O' || x == 'u' || x == 'U' || x == 'y' || x == 'Y'= x : filterVowels xs
            | otherwise = filterVowels xs

tasty :: Book -> Int
tasty book = div ((getPages book) * (numberOfVowels $ getTitle book)) (getTitleLength book)

booksToEat :: [Book] -> Int -> [String]
booksToEat books boundary = map (\book -> getTitle book) $ filter (\book -> tasty book >= boundary) books