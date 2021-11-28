module Main where

import Parser (Expr(..), Environment, parseEnvironment)
import Text.Parsec
import LambdaToSKI (compile)
import GraphReduction (run)
import           System.IO            (hSetEncoding, stdin, stdout, utf8)

main :: IO ()
main = do
  hSetEncoding stdin  utf8
  hSetEncoding stdout utf8
  putStrLn testSource

  run testSource
  putStrLn ""




testSource =
--     "f = \\x y -> + x 3 \n"
--  ++ "g = λx. * x 7\n"
--  ++ "fact = λn. if (is0 n) 1 (* n (fact (sub1 n))) \n"
--  ++ "compose = λf g. f g \n"
--  ++ "main = fact 7"

-- "main = c i 2 (+ 1)"
   "main = s k i 4 \n"

-- testSource =
--    "fact = y(λf n. if (eq 0 n) 1 (* n (f (sub n 1)))) \n" ++
--    "main = fact 0 \n"


--testSource = "id = \\x -> x \n" ++
--             "1 = \\f x -> f x \n" ++
--             "main = id 1"

-- testSource = "true = \\x y -> x \n" ++
--             "false = \\x y -> y \n" ++
--             "0 = \\f x -> x \n" ++
--             "1 = \\f x -> f x \n" ++
--             "succ = \\n f x -> f(n f x) \n" ++
--             "pred = \\n f x -> n(\\g h -> h (g f)) (\\u -> x) (\\u ->u) \n" ++
--             "mul = \\m n f -> m(n f) \n" ++
--             "is0 = \\n -> n (\\x -> false) true \n" ++
--             "Y = \\f -> (\\x -> x x)(\\x -> f(x x)) \n" ++
--             "fact = Y(\\f n -> (is0 n) 1 (mul n (f (pred n)))) \n" ++
--             "main = fact (succ (succ (succ 1))) \n"