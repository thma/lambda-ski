
{-# LANGUAGE QuasiQuotes #-}
module Main where

import           Control.Monad.ST
import Control.Category ( (>>>) )
import           Data.List        (lookup)
import           Data.Maybe
import           Data.STRef
import           GraphReduction
import           LambdaToSKI
import           CLTerm
import           Parser           (Environment, Expr(..), parseEnvironment)
import           System.IO        (hSetEncoding, stdin, stdout, utf8)
import HhiReducer
import Kiselyov
import System.TimeIt
import Text.RawString.QQ  

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
  hSetEncoding stdin utf8 -- this is required to handle UTF-8 characters like λ
  hSetEncoding stdout utf8 -- this is required to handle UTF-8 characters like λ

  -- testSource <-readFile "test/tak.ths"
  let testSource = "main = (\\x y -> + x x) 3 4"
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

  let kiselyov = compileKi env optK
  putStrLn "The result of the kiselyov compiler K opt:"
  print kiselyov

  let kiselyov' = compileKi env optEta
  putStrLn "The result of the kiselyov compiler Eta opt:"
  print kiselyov'

  let graph' = allocate kiselyov
  putStrLn "The allocated graph:"
  putStrLn $ runST $ printGraph graph'

  let reducedGraph' = reduceGraph graph'
  putStrLn "The result after reducing the graph:"
  putStrLn $ runST $ printGraph reducedGraph'

  timeIt $ print $ tak 60 10 5

  let pEnv = parseEnvironment tak'
      aExp = compileBulk pEnv

  timeIt $ print $ transLink primitives aExp

  let aExp' = compileEta pEnv
  timeIt $ print $ transLink primitives aExp'

runTest :: SourceCode -> String
runTest src =
  let pEnv = parseEnvironment src
      aExp = compileBulk pEnv
      --tExp = translate aExp
  in show $ transLink primitives aExp --link primitives tExp

tak :: Integer -> Integer -> Integer -> Integer
tak x y z = if y >= x then z else tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)

tak' :: SourceCode
tak' = [r| 
  expected = 4
  tak  = y(λf x y z -> (if (geq y x) z (f (f (sub1 x) y z) (f (sub1 y) z x) (f (sub1 z) x y ))))
  main = tak 60 10 5
|]

  --demo

type SourceCode = String

loadTestCase :: String -> IO CL
loadTestCase name = do
  src <- readFile $ "test/" ++ name ++ ".ths"
  putStrLn "The source: "
  putStrLn src
  let pEnv = parseEnvironment src
      expr = compile pEnv abstractToSKI
  return expr

graphReductionDemo :: IO CL -> IO ()
graphReductionDemo ioexpr = do
  expr <- ioexpr
  let graph = allocate expr      
      result = reduceGraph graph
      actual = runST $ printGraph result
  putStrLn "allocated graph:"
  print expr
  putStrLn "after graph reduction:"
  print actual


hhiReductionDemo :: IO CL -> IO ()
hhiReductionDemo ioexpr = do
  expr <- ioexpr
  putStrLn "compiled to CExpr"
  print expr
  let actual = transLink primitives expr
  putStrLn "after graph reduction:"
  print actual

demo :: IO ()
demo = do
  let testCases =
       [
         "factorial"
       , "fibonacci"
       , "tak"
       , "ackermann"
       , "gaussian"
       ]
  putStrLn "Graph-Reduction"
  mapM_ (loadTestCase >>> graphReductionDemo) testCases

  putStrLn "HHI Reduction"
  mapM_ (loadTestCase >>> hhiReductionDemo) testCases
