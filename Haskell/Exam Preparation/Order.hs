data Order = Online Float Int Int | Offline Float

instance Show Order where
    show (Online price number delivery) = "Online: " ++ show (price, number, delivery)
    show (Offline price)                = "Offline: " ++ show price

instance Eq Order where
    (==) (Online price1 number1 delivery1) (Online price2 number2 delivery2) = price1 == price2 && number1 == number2 && delivery1 == delivery2
    (==) (Offline price1) (Offline price2) = price1 == price2

isOnline :: Order -> Bool
isOnline (Online _ _ _) = True
isOnline (Offline _) = False

timeUntilReceiving :: Order -> Int
timeUntilReceiving (Online _ _ delivery) = delivery

getPrice :: Order -> Float
getPrice (Online price _ _) = price
getPrice (Offline price) = price

type Orders = [Order]

totalPrice :: Orders -> Float
totalPrice orders = sum $ map (\x -> getPrice x) orders

getOrders :: Order -> Int
getOrders (Online _ _ delivery) = delivery

onlineOrders :: Orders -> Int
onlineOrders orders = sum $ map (\order -> getOrders order) orders

isExpensive :: Order -> Bool
isExpensive = (>=100).getPrice