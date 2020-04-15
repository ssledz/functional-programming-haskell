"Infinite Data Structures"

let ones = 1:ones
head ones
take 3 ones

let ints = [1..]
take 3 ints

let as = repeat 'a'
head as
take 3 as

let fibs = 1:1:(zipWith (+) fibs (tail fibs))

head fibs
"fibs: " ++ show (take 10 fibs)

properfactors x = filter (\y->(x `mod` y == 0)) [2..(x-1)]

numproperfactors x = length (properfactors x)

primes = filter (\x-> (numproperfactors x == 0)) [2..]

"primes: " ++ show (take 10 primes)
