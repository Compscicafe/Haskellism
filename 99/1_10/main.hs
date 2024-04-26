myLast :: [a] ->a
myLast [x] = x
myLast (_:xs) = myLast xs

in_range :: Ord a => a -> a -> a -> Bool
in_range min max x = x>=min && x<max

reverse_:: [a] -> [a]
reverse_ [] = []
reverse_ (x:xs) = reverse_ xs ++ [x]

empt :: [a] -> Bool
empt [] = True
empt _ = False

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

merge:: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] list2 = list2
merge list1 [] = list1
merge(x:xs) (y:ys)
    | x <=y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
fact :: (Ord t, Num t) => t -> t
fact n
    | n <=1 = 1
    | otherwise = n*fact(n-1)


myButLast [x,_]  = x
myButLast (_:xs) = myButLast xs

elementAt list i = list !! (i-1)



numele [] = 0
numele (_:xs) =1+ numele(xs)

--data Mag = Mag String Int [String] deriving (Show)
encode :: Eq a => [a] -> [(Int,a)]
encode [] = []
encode (x: xs) = (length ( x : takeWhile(==x) xs), x): encode (dropWhile(==x) xs)

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x: xs) = (x: takeWhile(==x) xs) : pack(dropWhile(==x) xs)

compres [] = []
compres (x: xs) = x: compres(dropWhile(==x) xs)
--recursive solution

compres_ (x:ys@(y:_))
    |x==y = compres_ ys
    |otherwise = x : compres_ ys
compres_ ys = ys




