myLast :: [a] ->a
myLast [x] = x
myLast (_:xs) = myLast xs

in_range :: Ord a => a -> a -> a -> Bool
in_range min max x = x>=min && x<max


fact :: (Ord t, Num t) => t -> t
fact n 
    | n <=1 = 1
    | otherwise = n*fact(n-1)

is_zero 0 = True
is_zero _ = False



head':: [Num] => Num
    | = head
    