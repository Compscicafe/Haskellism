
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
