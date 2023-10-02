
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
import qualified Data.Bifunctor


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

  --let testSource = "main = (\\x y -> + x x) 3 4"
  mapM_ showCompilations [prod, factorial] --, fibonacci, ackermann, tak]
  --demo

type SourceCode = String

prod :: SourceCode
prod = "main = λx y. * x y"

tak :: SourceCode
tak = [r| 
  tak  = y(λf x y z. (if (geq y x) z (f (f (sub1 x) y z) (f (sub1 y) z x) (f (sub1 z) x y ))))
  main = tak 7 4 2 --18 6 3
|]

ackermann :: SourceCode
ackermann = [r|
  ack  = y(λf n m. if (is0 n) (+ m 1) (if (is0 m) (f (sub1 n) 1) (f (sub1 n) (f n (sub1 m)))))
  main = ack 2 2
|]

factorial :: SourceCode
factorial = [r| 
  fact = y(λf n. if (is0 n) 1 (* n (f (sub1 n))))
  main = fact 100
|]

fibonacci :: SourceCode
fibonacci = [r| 
  fib  = y(λf n. if (is0 n) 1 (if (eql n 1) 1 (+ (f (sub1 n)) (f (sub n 2)))))
  main = fib 10
|]

showCompilations :: SourceCode -> IO ()
showCompilations source = do
  let env = parseEnvironment source
  putStrLn "The parsed environment of named lambda expressions:"
  mapM_ print env
  putStrLn ""
  putStrLn "The main expression in de Bruijn notation:"
  mapM_ (print . Data.Bifunctor.second deBruijn) env

  let expr = compile env abstractToSKI
  putStrLn "The main expression compiled to SICKBY combinator expressions by recursice bracket abstraction:"
  print expr
  putStrLn ""

  putStrLn "applying plain Kiselyov compilation:"
  print $ compilePlain env
  putStrLn ""

  let expr' = compileEta env
  putStrLn "The main expression compiled to SICKBY combinator expressions with eta optimization:"
  print expr'
  putStrLn ""

  let expr'' = compileBulk env
  putStrLn "The main expression compiled to SICKBY combinator expressions with bulk combinators:"
  print expr''
  putStrLn ""

  let expr''' = compileBulkLinear env
  putStrLn "The main expression compiled to SICKBY combinator expressions with bulk combinators and linear elimination:"
  print expr'''
  putStrLn ""

  let expr'''' = compileBulkLog env
  putStrLn "The main expression compiled to SICKBY combinator expressions with bulk combinators and logarithmic elimination:"
  print expr''''
  putStrLn ""


hhiReductionDemo :: IO CL -> IO ()
hhiReductionDemo ioexpr = do
  expr <- ioexpr
  putStrLn "compiled to CExpr"
  print expr
  let actual = transLink primitives expr
  putStrLn "after graph reduction:"
  print actual


