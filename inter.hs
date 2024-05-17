import Control.Monad.RWS (MonadReader(reader))
import System.IO
import Data.Char
import Distribution.Simple.Utils (xargs)

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (==0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row -1) >= num

move:: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
	where update r n = if r == row then n-num else n

readNumber :: IO Int
readNumber = do n <- getLine
                return (read n :: Int)

adder :: IO ()
adder = do putStr "How many numbers? "
           n <- readNumber
           xs <- sequence [readNumber | _ <- [1..n]]
           putStrLn $ "The total is " ++ show (sum xs) 

last' :: [a] -> a
last' [x] = x 
last' [] = error "empty list"
last' (_:xs) = last' xs

preLast :: [a] -> a
preLast [x] = error "only one element"
preLast [] = error "empty list"
preLast [x,_] = x
preLast (_:xs) = preLast xs


withoutlast:: [a] -> [a]
withoutlast [] = []
withoutlast [x] = []
withoutlast (x:xs) = x:withoutlast xs

data Op = Add | Sub | Mul | Div | Pow
data Expr = Val Int | App Op Expr Expr
valid:: Op -> Int -> Int -> Bool
	valid Add x y = x <= y
	valid Sub x y = x > y
	valid Mul x y = x /= 1 && y /= 1 && x <= y
	valid Div x y = y /= 1 && x `mod` y == 0

apply:: Op -> Int -> Int -> Int
