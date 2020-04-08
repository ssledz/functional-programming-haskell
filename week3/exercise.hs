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

"Grow a Tree"

data Tree = Leaf | Node Int Tree Tree deriving Show

Leaf

let l = Node 1 Leaf Leaf

:t l

:set +m

treeDepth tree = case tree of
  Leaf -> 0
  Node _ l r -> 1 + max (treeDepth l) (treeDepth r)

treeDepth Leaf
treeDepth l

treeSum tree = case tree of
  Leaf -> 0
  Node x l r -> x + (treeSum l) + (treeSum r)

treeSum Leaf
treeSum l
treeSum $ Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf (Node 4 Leaf Leaf))


isSortedTree tree minVal maxVal = case tree of
  Leaf -> True
  Node x l r -> let ls = isSortedTree l minVal x
                    rs = isSortedTree r x maxVal
                in x >= minVal && x <= maxVal && ls && rs


isSortedTree Leaf minBound maxBound
isSortedTree (Node 1 Leaf Leaf) minBound maxBound
isSortedTree (Node 1 (Node 0 Leaf Leaf) Leaf) minBound maxBound
isSortedTree (Node 1 (Node 2 Leaf Leaf) Leaf) minBound maxBound


addNewMax tree = case tree of
  Leaf -> Node 0 Leaf Leaf
  Node x l Leaf -> Node x l (Node (x + 1) Leaf Leaf)
  Node x l r -> Node x l (addNewMax r)

addNewMax Leaf
addNewMax $ addNewMax Leaf
addNewMax $ addNewMax $ addNewMax Leaf
addNewMax $ addNewMax $ addNewMax $ addNewMax Leaf

