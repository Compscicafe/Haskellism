import Data.Char
import Prelude
-- sum of squares
sum_of_squares:: Int -> Int
sum_of_squares n = sum[x*x| x<- [1..n]]


grid:: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m] , y <- [0..n]]


third:: [a] -> a
third xs = head (tail (tail xs))
-- tail: return the list without the first element
-- head: return the first element of thelist
-- 
third_ :: [a] -> a
third_ xs = xs !!2
-- !!: return the element at the index
third__:: [a] -> a
third__ (_:_:x:_) = x


safetail xs = if null xs then [] else tail xs

safetail_ xs | null xs = []
                | otherwise = tail xs
safetail__ [] = []
safetail__(__:xs) = xs


leap n = if mod n 4 == 0 then True else False


-- palindrome
palindrome xs = reverse xs == xs
palindrome_ [] = True
palindrome_ [_] = True
palindrome_ xs = head xs == last xs && palindrome_ (init (tail xs))

--panagram
panagram :: Foldable t => t Char -> Bool
panagram xs = null [x | x <- ['a'..'z'], not (elem x xs)]

sort:: Ord a => [a] -> [a]
sort [] = []
sort (x:y:xs) = if x > y then y : sort (x:xs) else x : sort (y:xs)

-- bubble
doit [] =[]
doit(x:xs) | null xs = [x]
            | x > head xs = head xs : doit (x:tail xs)
            | otherwise = x : doit xs




doit_(x:xs) | x > head xs = head xs:doit (x:tail:xs)
            | otherwise = x:doit xs

bubblesort xs = foldl (\acc x -> doit acc) xs xs


-- binarysearch
binarysearch:: Ord a => [a] -> a -> Bool
binarysearch [] _ = False
binarysearch [x] y = x == y
binarysearch xs y = let n = length xs
                        m = n `div` 2
                    in if xs !! m == y then True
                        else if xs !! m > y then binarysearch (take m xs) y
                        else binarysearch (drop m xs) y
sequentialsearch:: Eq a => [a] -> a -> Bool
sequentialsearch [] _ = False
sequentialsearch (x:xs) y = if x == y then True else sequentialsearch xs y

-- quicksort
quicksort:: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

-- insertion sort
insertionsort:: Ord a => [a] -> [a]
insertionsort [] = []
insertionsort (x:xs) = insert x (insertionsort xs)
    where insert x [] = [x]
          insert x (y:ys) = if x <= y then x:y:ys else y:insert x ys


--all pairs of distince numbers lower than n
pairs n  = [(x,y) | x <- [1..n], y <- [1..n], x /= y]
--takes two lists in ascending order, and determines whether or not they have an
disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint [] _ = False
disjoint _ [] = False
disjoint (x:xs) (y:ys) | x == y = True
                        | x < y = disjoint xs (y:ys)
                        | otherwise = disjoint (x:xs) ys

-- floor square root
fqrt :: Float -> Integer
fqrt n = floor (sqrt n)

-- prime
primeRec:: Integer-> Integer -> Bool
primeRec n d
    | d >= n = True
    | n `mod` d == 0 = False
    | otherwise = primeRec n (d+1)

prime:: Integer -> Bool
prime n = primeRec n 2


-- gcd
gcd_:: Integer -> Integer -> Integer
gcd_ a b | a == b = a
        | a > b = gcd (a-b) b
        | otherwise = gcd a (b-a)
-- coprimes
coprime:: Integer -> Integer -> Bool
coprime a b = gcd a b == 1

allprimes :: Integer -> [Integer]
allprimes n = [x | x <- [2..n], prime x]


primefactors:: Integer -> [Integer]
primefactors n = [x| x <- [2..n], prime x, n `mod` x == 0]

primeR:: Integer -> Integer -> [Integer]
primeR n m = [x| x<-[n..m], prime x]


-- merge sort

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y then x:merge xs (y:ys) else y:merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort ys) (mergesort zs)
    where (ys, zs) = halve xs

-- quicksort
quicksort_ :: Ord a => [a] -> [a]
quicksort_ [] = []
quicksort_ (x:xs) = quicksort_ [y | y <- xs, y <= x] ++ [x] ++ quicksort_ [y | y <- xs, y > x]


numconstruct:: [Integer] -> Integer
numconstruct xs = read (foldl (\acc x -> acc ++ show x) "" xs)
-- 
