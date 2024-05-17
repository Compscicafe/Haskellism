sum:: [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

