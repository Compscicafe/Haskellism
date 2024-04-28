pyth:: Int -> [(Int,Int,Int)]
pyth n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

perfect:: Int -> [Int] --no factors and innit
perfect n = [x | x <- [1..n], sum [y | y <- [1..x-1], x `mod` y == 0] == x]

fib n = fibs !! n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)



halve:: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
    where n = length xs `div` 2

third:: [a] -> a
third xs = head (drop 1 (drop 1 xs))

third_:: [a] ->a
third_ xs = xs !! 2

third__:: [a] ->a
third__ xs = head(tail(tail xs))

third___:: [a] ->a
third___ (_:_:x:_) = x

safetail:: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs
safetail_:: [a] -> [a]
safetail_ xs = if null xs then [] else tail xs