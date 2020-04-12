maxhelper :: Int -> [Int] -> Int
maxhelper x [] = x
maxhelper x (y:ys) = maxhelper (if x < y then y else x) ys
max' :: [Int] -> Maybe Int
max' [] = Nothing
max' (x:xs) = Just (maxhelper x xs)
