-- FP Serie 01 | Gruppe 07
-- Milan Kai - 222201385
-- Jannes Peter - 221201486
-- Johannes Kortmann - 222201678

-- Aufgabe 1

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual a b c d = (a == b) && (b == c) && (c == d)

-- Aufgabe 2

type Triple = (Int, Int, Int)

orderTriple :: Triple -> Triple
orderTriple (x,y,z) | (x < y) && (y < z) = (x,y,z)
                    | x > y = orderTriple(y,x,z)
                    | y > z = orderTriple(x,z,y)

-- Aufgabe 3

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

-- Aufgabe 4

isElem :: Int -> [Int] -> Bool
isElem e [] = False
isElem e [a] = e == a
isElem e (x:xs) = e==x || isElem e xs

-- Aufgabe 5
-- a 

luhnDouble :: Int -> Int
luhnDouble x | a > 9 = (2 * x) - 9
             | otherwise = a
             where a = 2 * x
-- b

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0
-- with list comprehension
-- luhn a b c d = sum ([ luhnDouble n | n <- [c,a]] ++ [b,d]) `mod` 10 == 0
