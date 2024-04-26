replicant :: (Num i, Ord i) => i -> a -> [a]
replicant n x
    | n <=0 = []
    | otherwise = x : replicant (n-1) x


dupli [] = []
dupli (x:xs) = x:x:dupli xs

repli:: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

drop_:: [a] -> n ->[a]
drop_ (x:xs) n
    | n == 0 =
