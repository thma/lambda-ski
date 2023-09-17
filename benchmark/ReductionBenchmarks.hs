module ReductionBenchmarks where

import Criterion.Main ( defaultMain, bench, nf )
import Parser ( parseEnvironment, Expr(Int, App) )
import LambdaToSKI ( abstractToSKI, compile )
import CLTerm
import Kiselyov ( compileBulk, compileEta )
import GraphReduction ( allocate, normalForm, toString, Graph )
import Data.Maybe (fromJust)
import Data.STRef ( STRef )
import Control.Monad.ST ( ST, runST )
import HhiReducer
import Control.Monad.Fix ( fix )
import BenchmarkSources

loadTestCase :: SourceCode -> IO CL
loadTestCase src = do
  let pEnv = parseEnvironment src
      expr = compile pEnv abstractToSKI
  return expr

loadTestCaseBulk :: SourceCode -> IO CL
loadTestCaseBulk src = do
  let pEnv = parseEnvironment src
      expr = compileBulk pEnv
  return expr

loadTestCaseEta :: SourceCode -> IO CL
loadTestCaseEta src = do
  let pEnv = parseEnvironment src
      expr = compileEta pEnv
  return expr  

graphTest :: CL -> String
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

reducerTest :: CL -> String
reducerTest expr = show $ transLink primitives expr

reducerTestLog :: CL -> String
reducerTestLog expr = show $ transLinkLog primitives expr

benchmarks :: IO ()
benchmarks = do
  fac <- loadTestCase factorial
  fib <- loadTestCase fibonacci
  akk <- loadTestCase ackermann
  gau <- loadTestCase gaussian
  tak <- loadTestCase tak
  facEta <- loadTestCaseEta factorial
  fibEta <- loadTestCaseEta fibonacci
  akkEta <- loadTestCaseEta ackermann
  gauEta <- loadTestCaseEta gaussian
  takEta <- loadTestCaseEta BenchmarkSources.tak
  facBulk <- loadTestCaseBulk factorial
  fibBulk <- loadTestCaseBulk fibonacci
  akkBulk <- loadTestCaseBulk ackermann
  gauBulk <- loadTestCaseBulk gaussian
  takBulk <- loadTestCaseBulk BenchmarkSources.tak

  -- sanity checks
  print $ graphTest fac
  print $ reducerTest fac
  print $ reducerTest facEta
  print $ reducerTest facBulk
  print $ reducerTestLog facBulk
  print $ show $ fact 100

  defaultMain [
        bench "factorial Graph-Reduce"    $ nf graphTest fac
      , bench "factorial HHI-Reduce"      $ nf reducerTest fac
      , bench "factorial HHI-Eta"         $ nf reducerTest facEta
      , bench "factorial HHI-Bulk"        $ nf reducerTest facBulk
      , bench "factorial HHI-Bulk-Log"    $ nf reducerTestLog facBulk
      , bench "factorial Native"          $ nf fact 100
      , bench "fibonacci Graph-Reduce"    $ nf graphTest fib
      , bench "fibonacci HHI-Reduce"      $ nf reducerTest fib
      , bench "fibonacci HHI-Eta"         $ nf reducerTest fibEta
      , bench "fibonacci HHi-Bulk"        $ nf reducerTest fibBulk
      , bench "fibonacci HHI-Bulk-Log"    $ nf reducerTestLog fibBulk
      , bench "fibonacci Native"          $ nf fibo 10
      , bench "ackermann Graph-Reduce"    $ nf graphTest akk
      , bench "ackermann HHI-Reduce"      $ nf reducerTest akk
      , bench "ackermann HHI-Eta"         $ nf reducerTest akkEta
      , bench "ackermann HHI-Bulk"        $ nf reducerTest akkBulk
      , bench "ackermann HHI-Bulk-Log"    $ nf reducerTestLog akkBulk
      , bench "ackermann Native"          $ nf ack_2 2
      , bench "gaussian  Graph-Reduce"    $ nf graphTest gau
      , bench "gaussian  HHI-Reduce"      $ nf reducerTest gau
      , bench "gaussian  HHI-Eta"         $ nf reducerTest gauEta
      , bench "gaussian  HHI-Bulk"        $ nf reducerTest gauBulk
      , bench "gaussian  HHI-Bulk-Log"    $ nf reducerTestLog gauBulk
      , bench "gaussian  Native"          $ nf gaussianSum 100
      , bench "tak       Graph-Reduce"    $ nf graphTest tak
      , bench "tak       HHI-Reduce"      $ nf reducerTest tak
      , bench "tak       HHI-Eta"         $ nf reducerTest takEta
      , bench "tak       HHI-Bulk"        $ nf reducerTest takBulk
      , bench "tak       HHI-Bulk-Log"    $ nf reducerTestLog takBulk
      , bench "tak       Native"          $ nf tak1 (7,4,2) 
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


tak_18_6 :: Integer -> Integer
tak_18_6 = takN 18 6

takN :: Integer -> Integer -> Integer -> Integer
takN  = fix (\f x y z -> (if y >= x then z else f (f (x-1) y z) (f (y-1) z x) (f (z-1) x y )))

tak1 (x,y,z) = takN x y z


