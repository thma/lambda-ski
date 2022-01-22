module Main where

-- (allocate, toString, Graph, step, normalForm, nf)

-- (compile, abstractToSKI, babs, babs0, ropt)

import           Control.Monad.ST
import           Data.List        (lookup)
import           Data.Maybe
import           Data.STRef
import           GraphReduction
import           LambdaToSKI
import           Parser           (Environment, Expr(..), parseEnvironment)
import           System.IO        (hSetEncoding, stdin, stdout, utf8)

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

type SourceCode = String

loadTestCase :: String -> IO SourceCode
loadTestCase name = readFile $ "test/" ++ name ++ ".ths"

getInt :: Expr -> Integer 
getInt (Int i) = i
getInt _ = error "not an int"

--runTest :: SourceCode -> Bool
runTest src = do
  let pEnv = parseEnvironment src
      expr = compile pEnv abstractSimple
      graph = allocate expr
      expected = show $ getInt $ fromJust (lookup "expected" pEnv)
      result = reduceGraph graph
      actual = runST $ printGraph result
  
  print expected
  print actual

demoG :: IO ()
demoG = do
  src <- loadTestCase "factorial"
  runTest src