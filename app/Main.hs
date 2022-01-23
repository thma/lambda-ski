module Main where

-- (allocate, toString, Graph, step, normalForm, nf)

-- (compile, abstractToSKI, babs, babs0, ropt)

import           Control.Monad.ST
import           Control.Category
import           Data.List        (lookup)
import           Data.Maybe
import           Data.STRef
import           GraphReduction
import           LambdaToSKI
import           Parser           (Environment, Expr(..), parseEnvironment)
import           System.IO        (hSetEncoding, stdin, stdout, utf8)
import HhiReducer

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

  testSource <-readFile "test/tak.ths"
  putStrLn "The sourcecode: "
  putStrLn testSource

  let env = parseEnvironment testSource
  putStrLn "The parsed environment of named lambda expressions:"
  mapM_ print env
  putStrLn ""

  let expr = compile env abstractSimple --abstractToSKI
  putStrLn "The main expression compiled to SICKYB combinator expressions:"
  print expr
  putStrLn ""

  let graph = allocate expr
  putStrLn "The allocated graph:"
  putStrLn $ runST $ printGraph graph

  let reducedGraph = reduceGraph graph

  putStrLn "The result after reducing the graph:"
  putStrLn $ runST $ printGraph reducedGraph

  demo

type SourceCode = String

loadTestCase :: String -> IO Expr
loadTestCase name = do
  src <- readFile $ "test/" ++ name ++ ".ths"
  putStrLn "The source: "
  putStrLn src
  let pEnv = parseEnvironment src
      expr = compile pEnv abstractSimple
  return expr

graphReductionDemo :: IO Expr -> IO ()
graphReductionDemo ioexpr = do
  expr <- ioexpr
  let graph = allocate expr      
      result = reduceGraph graph
      actual = runST $ printGraph result
  putStrLn "allocated graph:"
  print expr
  putStrLn "after graph reduction:"
  print actual


hhiReductionDemo :: IO Expr -> IO ()
hhiReductionDemo ioexpr = do
  expr <- ioexpr
  let cexpr = translate expr
  putStrLn "compiled to CExpr"
  print cexpr
  let actual = link primitives cexpr
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
