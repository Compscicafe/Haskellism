
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


addThree :: Int -> Int -> Int -> Int
addThree x y z = x+y+z

circumference :: Float -> Float
circumference r = 2 * pi * r

desne :: Double -> Double
desne r = pi * 23*r
