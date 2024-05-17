import Data.Char
import Data.List
import System.IO ()
halve :: [a1] -> ([a1], [a1])
halve [] = ([], [])
halve (x:xs) = (x:ys, zs)
    where (zs, ys) = halve xs
third :: [a] -> a
third xs = head (tail (tail xs))
third' :: [a] -> a
third' [] = error "empty list"
third' [_] = error "only one element"
third' [x,_] = error "only two elements"
third' (_:_:x:_) = x
third'' :: [a] -> a
third'' xs = xs !! 2
safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs
safetail' :: [a] -> [a]
safetail' xs
    | null xs = []
    | otherwise = tail xs
safetail'' :: [a] -> [a]
safetail'' xs = if null xs then [] else tail xs
safetail''' :: [a] -> [a]
safetail''' = \ xs -> case xs of
    [] -> []
    (_:xs) -> xs
--The catch all condition otherwise is defined in
--the prelude by otherwise = True
--curried 
odds n = map (\x -> x*2 +1) [0..n-1]
luhnDouble :: Int -> Int
luhnDouble x
    | double > 9 = double - 9
    | otherwise = double
    where double = x * 2
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = total `mod` 10 == 0
    where total = sum (map luhnDouble [a,b,c,d])
    
pairs:: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

let2int :: Char->Int
let2int c = ord c - ord 'a'
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)
shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let ((let2int c + n) `mod` 26)
    | isUpper c = toUpper (int2let ((let2int (toLower c) + n) `mod` 26))
    | otherwise = c
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
decode :: Int -> String -> String
decode n xs = encode (-n) xs
perfect:: Int -> [Int] --no factors and innit
perfect n = [x | x <- [1..n], sum [y | y <- [1..x-1], x `mod` y == 0] == x]
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']
grid:: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]
find' :: Eq a => a -> [(a,b)] -> [b]
find' k t = [v | (k',v) <- t, k == k']
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find' x (zip xs [0..])
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys
mergesort:: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort (fst halves)) (mergesort (snd halves))
    where halves = halve xs
euclid :: Int -> Int -> Int
euclid x y
    | x == y = x
    | x > y = euclid (x-y) y
    | otherwise = euclid x (y-x)
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

length' :: [a] -> Int
length' = foldr (\_ n -> n+1) 0
reverse' :: [a] -> [a]
reverse' = foldl (\xs x -> x:xs) []
product' :: Num a => [a] -> a
product' = foldr (*) 1

--higher order functions
all :: (a -> Bool) -> [a] -> Bool
all p xs = and (map p xs)
any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or (map p xs)
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x = x:takeWhile' p xs
    | otherwise = []
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
    | p x = dropWhile' p xs
    | otherwise = x:xs
map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x xs -> f x:xs) []
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []
dec2int :: [Int] -> Int
dec2int = foldl (\n x -> n*10 + x) 0
curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = \x y -> f (x,y)
uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f = \(x,y) -> f x y
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
    | p x = []
    | otherwise = h x : unfold p h t (t x)
palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs
morse :: [Char] -> [Char]
morse xs = concat (map (find xs) morseTable)
    where
        morseTable = [('A', ".-"), ('B', "-..."), ('C', "-.-."), ('D', "-.."), ('E', "."), ('F', "..-."), ('G', "--."), ('H', "...."), ('I', ".."), ('J', ".---"), ('K', "-.-"), ('L', ".-.."), ('M', "--"), ('N', "-."), ('O', "---"), ('P', ".--."), ('Q', "--.-"), ('R', ".-."), ('S', "..."), ('T', "-"), ('U', "..-"), ('V', "...-"), ('W', ".--"), ('X', "-..-"), ('Y', "-.--"), ('Z', "--..")]
        find xs (a,b) = if elem a xs then b else ""
catalan :: [Int] -> [Int]
catalan xs = map catalan' xs
    where
        catalan' 0 = 1
        catalan' n = sum [catalan' i * catalan' (n-1-i) | i <- [0..n-1]]

scytale :: [Char] -> [Char]
scytale xs = concat (transpose (chunksOf 3 xs))
    where
        chunksOf n [] = []
        chunksOf n xs = take n xs : chunksOf n (drop n xs)
        transpose [] = []
        transpose xs
            | any null xs = []
            | otherwise = map head xs : transpose (map tail xs)
squish :: [[a]] -> [a]
squish xs = foldr (++) [] xs
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish (map f xs)
length = foldr (\_ n -> n+1) 0
squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\x y -> if f x y == GT then x else y) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\x y -> if f x y == LT then x else y) x xs
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare
myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

