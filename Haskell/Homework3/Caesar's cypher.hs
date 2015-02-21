import Data.Char
-- Caesar's cypher

-- 1-ви начин за кодиране
encode :: String -> Int -> String
encode str 0 = str
encode [] _ = []
encode (x : xs) offset = chr (ord x + offset) : encode xs offset

-- 2-ри начин за кодиране
encode2 :: String -> Int -> String
encode2 str 0 = str
encode2 [] _ = []
encode2 str offset = reverse $ iter str []
    where
        iter [] result = result
        iter (x : xs) result = iter xs $ chr (ord x + offset) : result

-- декодиране
decode :: String -> Int -> String
decode str 0 = str
decode [] _ = []
decode (x : xs) offset = chr (ord x - offset) : decode xs offset