module ReductionBenchmarks where

import Criterion.Main ( defaultMain, bench, nf )
import Parser ( parseEnvironment, Expr(Int) )
import LambdaToSKI ( abstractSimple, compile )
import GraphReduction ( allocate, normalForm, toString, Graph )
import Data.Maybe (fromJust)
import Data.STRef ( STRef )
import Control.Monad.ST ( ST, runST )
import Reducer ( link, primitives, translate )
import Control.Monad.Fix ( fix )

type SourceCode = String

loadTestCase :: String -> IO Expr
loadTestCase name = do
  src <- readFile $ "test/" ++ name ++ ".ths"
  let pEnv = parseEnvironment src
      expr = compile pEnv abstractSimple
  return expr

getInt :: Expr -> Integer
getInt (Int i) = i
getInt _ = error "not an int"


graphTest :: Expr -> String
graphTest expr =
  let graph = allocate expr
      result = reduceGraph graph
      actual = runST $ printGraph result
  in  actual


printGraph :: ST s (STRef s (Graph s)) -> ST s String
printGraph graph = do
  gP <- graph
  toString gP

reduceGraph :: ST s (STRef s (Graph s)) -> ST s (STRef s (Graph s))
reduceGraph graph = do
  gP <- graph
  normalForm gP

reducerTest :: Expr -> String
reducerTest expr =
  let tExp = translate expr
      actual = link primitives tExp
  in show actual


benchmarks :: IO ()
benchmarks = do
  fac <- loadTestCase "factorial"
  fib <- loadTestCase "fibonacci"
  akk <- loadTestCase "ackermann"
  gau <- loadTestCase "gaussian"
  tak <- loadTestCase "tak"

  defaultMain [
        bench "factorial Graph-Reduce"    $ nf graphTest fac
      , bench "factorial HHI-Reduce"      $ nf reducerTest fac
      , bench "factorial Native"          $ nf fact 100
      , bench "fibonacci Graph-Reduce"    $ nf graphTest fib
      , bench "fibonacci HHI-Reduce"      $ nf reducerTest fib
      , bench "fibonacci Native"          $ nf fibo 10
      , bench "ackermann Graph-Reduce"    $ nf graphTest akk
      , bench "ackermann HHI-Reduce"      $ nf reducerTest akk
      , bench "ackermann Native"          $ nf ack_2 2
      , bench "gaussian  Graph-Reduce"    $ nf graphTest gau
      , bench "gaussian  HHI-Reduce"      $ nf reducerTest gau
      , bench "gaussian  Native"          $ nf gaussianSum 100
      , bench "tak       Graph-Reduce"    $ nf graphTest tak
      , bench "tak       HHI-Reduce"      $ nf reducerTest tak
      , bench "tak       Native"          $ nf tak_6_4 3
      ]
  return ()



fact :: Integer -> Integer
fact = fix (\f n -> if n == 0 then 1 else n * f (n-1))

fibo :: Integer -> Integer
fibo  = fix (\f n -> if n == 0 || n == 1 then 1 else f (n-1) + f (n - 2))

ack_2 :: Integer -> Integer
ack_2 = ack 2

ack :: Integer -> Integer -> Integer
ack  = fix (\f n m ->
  if n == 0
    then m + 1
    else (if m == 0
      then f (n-1) 1
      else f (n-1) (f n (m-1))))

gaussianSum :: Integer -> Integer
gaussianSum  = fix (\f n -> if n == 0 then 0 else n + f (n-1))


tak_6_4 :: Integer -> Integer
tak_6_4 = tak 6 4

tak :: Integer -> Integer -> Integer -> Integer
tak  = fix (\f x y z -> (if y >= x then z else f (f (x-1) y z) (f (y-1) z x) (f (z-1) x y )))