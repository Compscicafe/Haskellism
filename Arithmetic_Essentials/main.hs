even :: Integral a => a -> Bool
even n = n `mod` 2 ==0

odd :: Integral a => a -> Bool
odd n = n `mod` 2 /= 0


doubleus :: Num a => a -> a -> a
doubleus x y = x + y + x + 2*y 

doubleSmall x = if x>10
                then x
                else x*2

doubleSmall' :: (Ord a, Num a) => a -> a
doubleSmall' x = (if x>10 then x else x*2) + 1
