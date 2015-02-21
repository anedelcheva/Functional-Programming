import Data.List

repeater :: String -> (Int -> String -> String)
repeater str = joining
    where
        joining count glue = intercalate glue $ replicate count str