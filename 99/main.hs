
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


addThree :: Int -> Int -> Int -> Int
addThree x y z = x+y+z

circumference :: Float -> Float
circumference r = 2 * pi * r

desne :: Double -> Double
desne r = pi * 23*r

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sidearea = 2* pi * r * h
        toparea = pi * r^2
    in sidearea +2 * toparea

calBMI :: (RealFloat a) => [(a,a)] -> [a]
calBMI xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]


maximum_ :: (Ord a) => [a]->a
maximum_ [] = error "NO MOREs"
maximum_ [x] = x
maximum_ (x:xs)
    | x > maxtail = x
    | otherwise = maxtail
    where maxtail = maximum_ xs

replicate_ :: (Num i, Ord i) => i -> a -> [a]
replicate_ n x
    | n <= 0 = []
    | otherwise = x:replicate_ (n-1) x

linearSearch :: (Int -> Int) -> Int -> [Int]
linearSearch f t = [x| x <- [0..t], t == f x]

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x*z*y

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n+2 ) else Nothing


ifOddMinus3 :: Integer -> Maybe Integer
ifOddMinus3 n = if odd n then Just (n-3) else Nothing




--palindrome(*) Find out whether a list is a palindrome.
palindrome xs = xs == reverse xs

data NestedList a = Elem a | List [NestedList a]

--flatten(*) Flatten a nested list structure.
flatten :: NestedList a -> [a]

flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- comrpess
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x:compress (dropWhile (==x) xs)
