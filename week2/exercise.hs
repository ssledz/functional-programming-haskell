"Do it Yourself: Lists and Recursion"
[]
1 : []
1 : 2 : []
x:xs = [1,2,3]
"head : " ++ show x
"tail : " ++ show xs

:set +m

let {
  length' lst
    | lst == [] = 0
    | otherwise = let x:xs = lst in 1 + length' xs
}

let ys = [1,2,3,4,5]

"length' [1,2,3,4,5] : " ++ show (length' ys)

length'' lst = if lst == [] then 0 else let x:xs = lst in 1 + length'' xs

"length'' [1,2,3,4,5] : " ++ show (length'' ys)

let f = f' where f' 1 = 0; f' x = 1 + f' (x - 1)

"f 1 = " ++ show (f 1)
"f 2 = " ++ show (f 2)

filter (<5) [1..10]

let {
  filter' f lst = case lst of
    [] -> []
    x:xs -> let ys = filter' f xs in if f x then x:ys else ys
}

filter' (<5) [1..10]

"Do it Yourself: Function Composition"

-- map f (map g xs) == map (f . g) xs

map (+5) (map (*3) [1..10])

map ((+5) . (*3)) [1..10]


revers' = foldl (\acc x -> x:acc) ""
revers' "Reversing a string"

sum xs = foldr (+) 0 xs
product xs = foldr (*) 1 xs


sum [1..10]
product [1..10]

revers'' = foldr (\x acc -> acc ++ [x]) ""
revers' "Reversing a string"



