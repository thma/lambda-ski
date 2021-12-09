module Main where

import           GraphReductionSTRef (run)
import           System.IO           (hSetEncoding, stdin, stdout, utf8)

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  putStrLn testSource
  putStrLn $ run testSource

testSource :: String
testSource =
  --     "f = \\x y -> + x 3 \n"
  --  ++ "g = λx. * x 7 \n"
  --  ++ "h = λx -> (compose f g) x \n"
  --  ++ "compose = λf g x. f (g x) \n"
  --  ++ "main = h 5"

  --   "main = c i 2 (+ 1)"
  --   "main = s k i 4 \n"

  --"main = if (sub 3 2) (* 4 4) (+ 5 5)"

  "Y = λf -> (λx -> x x)(λx -> f(x x)) \n"
    ++ "fact = y(λf n. if (is0 n) 1 (* n (f (sub1 n)))) \n"
    ++ "main = fact 10000 \n"

--testSource =
-- "add = λx y -> + x y\n" ++
-- "mul = λx y -> * x y\n" ++
-- "main = mul (add 2 3) (mul 7 6)"

-- testSource =
-- "true = λx y -> x\n" ++
-- "false = λx y -> y\n" ++
-- "zero = λf x -> x\n" ++
-- "one = λf x -> f x\n" ++
-- "succ = λn f x -> f(n f x)\n" ++
-- "pred = λn f x -> n(λg h -> h (g f)) (λu -> x) (λu ->u)\n" ++
-- "mul = λm n f -> m(n f)\n" ++
-- "is0 = λn -> n (λx -> false) true\n" ++
-- "Y = λf -> (λx -> x x)(λx -> f(x x))\n" ++
-- "fact = Y(λf n -> (is0 n) one (mul n (f (pred n))))\n" ++
-- "main = fact one \n" -- (succ (succ (succ one)))  -- Compute 4!\n"
