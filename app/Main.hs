module Main where

import           GraphReductionSTRef (run, allocate, toString, Graph, loop)
import           System.IO           (hSetEncoding, stdin, stdout, utf8)
import Parser (parseEnvironment, Environment, Expr)
import LambdaToSKI (compile, compileToSKI, compileToCCC)
import Data.STRef 
import Control.Monad.ST 

parseEnv :: String -> Environment
parseEnv source =
  case parseEnvironment source of
    Left err  -> error $ show err
    Right env -> env

compileEnv :: Environment -> (Environment -> Expr -> Expr) -> Expr
compileEnv env compileFun =
  case compile env compileFun of
    Left err      -> error $ show err
    Right expr    -> expr

printGraph :: ST s (STRef s (Graph s)) -> ST s String
printGraph graph = do
 gP <- graph
 toString gP

reduceGraph :: ST s (STRef s (Graph s)) -> ST s (STRef s (Graph s))
reduceGraph graph = do
  gP <- graph
  loop gP

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  putStrLn "The sourcecode: "
  putStrLn testSource

  let env = parseEnv testSource
  putStrLn "The parsed environment of named lambda expressions:"
  mapM_ print env
  putStrLn ""

  let expr = compileEnv env compileToSKI
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
    ++ "main = fact 100 \n"

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
