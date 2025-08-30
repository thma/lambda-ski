module Example where

--fix :: (Int -> Int) -> Int
fix f = f $ fix f

fib :: Int -> Int
fib  = fix (\f n -> if n <= 2 then 1 else f (n-1) + f (n - 2))

main = print (fib 10)