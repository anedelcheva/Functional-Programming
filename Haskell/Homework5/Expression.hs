import Data.Char

data Expression = Plus Int Int | Minus Int Int | Mult Int Int

instance Show Expression where
    show (Plus x y) = show (x + y)
    show (Minus x y) = show (x - y)
    show (Mult x y) = show (x * y)

instance Eq Expression where
    (==) (Plus x1 y1) (Plus x2 y2) = (x1 == x2 && y1 == y2) || (x1 == y2 && y1 == x2)
    (==) (Minus x1 y1) (Minus x2 y2) = (x1 == x2 && y1 == y2) || (x1 == y2 && y1 == x2)
    (==) (Mult x1 y1) (Mult x2 y2) = (x1 == x2 && y1 == y2) || (x1 == y2 && y1 == x2)
    (==) _ _ = False

evaluate :: Expression -> Int
evaluate (Plus x y) = x + y
evaluate (Minus x y) = x - y
evaluate (Mult x y) = x * y

toExpression :: String -> Maybe Expression
toExpression str
    | sign == "+" = Just (Plus operand1 operand2)
    | sign == "-" = Just (Minus operand1 operand2)
    | sign == "*" = Just (Mult operand1 operand2)
    | otherwise = Nothing
    where
        units = words str
        sign = units !! 1
        operand1 = read (units !! 0) :: Int
        operand2 = read (units !! 2) :: Int

evaluate2 :: Expression -> Maybe Int
evaluate2 (Plus x y) = Just (x + y)
evaluate2 (Minus x y) = Just (x - y)
evaluate2 (Mult x y) = Just (x * y)

str = "5 + 5"
str1 = "10 * 10"
str2 = "3 - 10"
str3 = "3 3 - 1"
str4 = "wtf haskell"
str5 = "5 ++ 5"