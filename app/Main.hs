module Main where

import           GraphReductionSTRef (allocate, toString, Graph, normalForm)
import           System.IO           (hSetEncoding, stdin, stdout, utf8)
import Parser (parseEnvironment, Environment, Expr)
import LambdaToSKI (compile, abstractToSKI, babs, ropt)
import Data.STRef 
import Control.Monad.ST 


printGraph :: ST s (STRef s (Graph s)) -> ST s String
printGraph graph = do
 gP <- graph
 toString gP

reduceGraph :: ST s (STRef s (Graph s)) -> ST s (STRef s (Graph s))
reduceGraph graph = do
  gP <- graph
  normalForm gP

main :: IO ()
main = do
  hSetEncoding stdin utf8   -- this is required to handle UTF-8 characters like λ
  hSetEncoding stdout utf8  -- this is required to handle UTF-8 characters like λ
  putStrLn "The sourcecode: "
  putStrLn testSource

  let env = parseEnvironment testSource
  putStrLn "The parsed environment of named lambda expressions:"
  mapM_ print env
  putStrLn ""

  let expr = compile env abstractToSKI
  putStrLn "The main expression compiled to SICKYB combinator expressions:"
  print expr
  putStrLn ""

  let graph = allocate expr
  putStrLn "The allocated graph:"
  putStrLn $ runST $ printGraph graph

  let reducedGraph = reduceGraph graph

  putStrLn "The result after reducing the graph:"
  putStrLn $ runST $ printGraph reducedGraph

testSource :: String
testSource =
    "main = (λx -> (+ x 4)) 5"

  --   "main = c i 2 (+ 1)"
  --   "main = s k i 4 \n"

  --"main = if (sub 3 2) (* 4 4) (+ 5 5)"

    --    "Y    = λf -> (λx -> x x)(λx -> f(x x)) \n"
    -- ++ "fact = Y(λf n. if (is0 n) 1 (* n (f (sub1 n)))) \n"
    -- ++ "main = fact 10 \n"

  --    "isEven = \\n -> eq (rem n 2) 0 \n"
  -- ++ "not    = \\b -> if (eq b 1) 0 1 \n"
  -- ++ "isOdd  = \\n -> not (isEven n) \n"
  -- ++ "main   = isOdd 23333 \n"  

  --    "isEven = λn -> or (is0 n) (isOdd (sub1 n)) \n"
  -- ++ "isOdd  = λn -> and (not (is0 n)) (isEven (sub1 n)) \n"
  -- ++ "not    = \\b -> if (eq b 1) 0 1 \n"
  -- ++ "or     = λx y -> if (eq x 1) 1 (if (eq y 1) 1 0) \n"  
  -- ++ "and    = λx y -> if (eq x 1) (if (eq y 1) 1 0) 0 \n"
  -- ++ "main   = isEven 0 \n"  

  {--
       (is-even? (lambda (n) (or (eq 0 n) (is-odd? (- n 1)))))
     (is-odd? (lambda (n) (and (not (eq 0 n)) (is-even? (- n 1)))))
  --}

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
