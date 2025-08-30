module ReductionBenchmarks where

import Criterion.Main ( defaultMain, bench, nf, nfIO )
import Parser ( parseEnvironment, Expr(Int, App) )
import LambdaToSKI ( compileBracket )
import CLTerm
import Kiselyov
import GraphReduction ( allocate, normalForm, toString, Graph )
import Data.Maybe (fromJust)
import Data.STRef ( STRef )
import Control.Monad.ST ( ST, runST )
import HhiReducer
import Control.Monad.Fix ( fix )
import BenchmarkSources
import MicroHsExp ( toMhsPrg )
import MhsEval 

loadTestCase :: SourceCode -> IO CL
loadTestCase src = do
  let pEnv = parseEnvironment src
      expr = compileBracket pEnv
  return expr

loadTestCaseBulk :: SourceCode -> IO CL
loadTestCaseBulk src = do
  let pEnv = parseEnvironment src
      expr = compileBulk pEnv
  return expr

loadTestCaseBreakBulkLinear :: SourceCode -> IO CL
loadTestCaseBreakBulkLinear src = do
  let pEnv = parseEnvironment src
      expr = compileBulkLinear pEnv
  return expr

loadTestCaseBreakBulkLog :: SourceCode -> IO CL
loadTestCaseBreakBulkLog src = do
  let pEnv = parseEnvironment src
      expr = compileBulkLog pEnv
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

microHsTest :: MhsContext -> String -> IO ()
microHsTest ctx prg = run ctx prg

  
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
  facBulkLinear <- loadTestCaseBreakBulkLinear factorial
  fibBulkLinear <- loadTestCaseBreakBulkLinear fibonacci
  akkBulkLinear <- loadTestCaseBreakBulkLinear ackermann
  gauBulkLinear <- loadTestCaseBreakBulkLinear gaussian
  takBulkLinear <- loadTestCaseBreakBulkLinear BenchmarkSources.tak
  facBulkLog <- loadTestCaseBreakBulkLog factorial
  fibBulkLog <- loadTestCaseBreakBulkLog fibonacci
  akkBulkLog <- loadTestCaseBreakBulkLog ackermann
  gauBulkLog <- loadTestCaseBreakBulkLog gaussian
  takBulkLog <- loadTestCaseBreakBulkLog BenchmarkSources.tak

  mhsContext <- createMhsContext
  let mhsFacEta = toMhsPrg facEta
      mhsTakEta = toMhsPrg takEta
      mhsFibEta = toMhsPrg fibEta
      mhsAckEta = toMhsPrg akkEta

  print facEta
  print takEta
  print fibEta
  print akkEta

  -- sanity checks
  print $ graphTest fac
  print $ graphTest facEta
  print $ reducerTest fac
  print $ reducerTest facEta
  print $ reducerTest facBulk
  print $ reducerTestLog facBulk
  print $ show $ fact 10
  do 
    microHsResult <- microHsTest mhsContext mhsFacEta
    print microHsResult

  defaultMain [
      --  bench "factorial Graph-Reduce"     $ nf graphTest fac
      -- , bench "factorial Graph-Reduce-Eta" $ nf graphTest facEta
      -- , bench "factorial Graph-Reduce-Lin" $ nf graphTest facBulkLinear
      -- , bench "factorial Graph-Reduce-Log" $ nf graphTest facBulkLog
      -- , bench "factorial HHI-Reduce"       $ nf reducerTest fac
        bench "factorial HHI-Eta"          $ nf reducerTest facEta
      -- , bench "factorial HHI-Bulk"         $ nf reducerTest facBulk
      -- , bench "factorial HHI-Bulk-Log"     $ nf reducerTestLog facBulk
      -- , bench "factorial HHI-Break-Bulk"   $ nf reducerTest facBulkLinear
      -- , bench "factorial HHI-Break-Log"    $ nf reducerTestLog facBulkLog
      , bench "factorial MicroHs"          $ nfIO (microHsTest mhsContext mhsFacEta)
      , bench "factorial Native"           $ nf fact 10
      -- bench "fibonacci Graph-Reduce"     $ nf graphTest fib
      --, bench "fibonacci Graph-Reduce-Eta" $ nf graphTest fibEta
      --, bench "fibonacci Graph-Reduce-Lin" $ nf graphTest fibBulkLinear
      --, bench "fibonacci Graph-Reduce-Log" $ nf graphTest fibBulkLog
      --, bench "fibonacci HHI-Reduce"       $ nf reducerTest fib
      , bench "fibonacci HHI-Eta"          $ nf reducerTest fibEta
      -- , bench "fibonacci HHi-Bulk"         $ nf reducerTest fibBulk
      -- , bench "fibonacci HHI-Bulk-Log"     $ nf reducerTestLog fibBulk
      -- , bench "fibonacci HHI-Break-Bulk"   $ nf reducerTest fibBulkLinear
      -- , bench "fibonacci HHI-Break-Log"    $ nf reducerTestLog fibBulkLog
      , bench "fibonacci MicroHs"          $ nfIO (microHsTest mhsContext mhsFibEta)
      , bench "fibonacci Native"           $ nf fibo 37
      -- , bench "ackermann Graph-Reduce"     $ nf graphTest akk
      --, bench "ackermann Graph-Reduce-Eta" $ nf graphTest akkEta
      -- , bench "ackermann Graph-Reduce-Lin" $ nf graphTest akkBulkLinear
      -- , bench "ackermann Graph-Reduce-Log" $ nf graphTest akkBulkLog
      , bench "ackermann HHI-Reduce"       $ nf reducerTest akkEta
      -- , bench "ackermann HHI-Eta"          $ nf reducerTest akkEta
      -- , bench "ackermann HHI-Bulk"         $ nf reducerTest akkBulk
      -- , bench "ackermann HHI-Bulk-Log"     $ nf reducerTestLog akkBulk
      -- , bench "ackermann HHI-Break-Bulk"   $ nf reducerTest akkBulkLinear
      -- , bench "ackermann HHI-Break-Log"    $ nf reducerTestLog akkBulkLog
      , bench "ackermann MicroHs"          $ nfIO (microHsTest mhsContext mhsAckEta)
      , bench "ackermann Native"           $ nf ack_3 9
      -- -- , bench "gaussian  Graph-Reduce"     $ nf graphTest gau
      -- -- , bench "gaussian  Graph-Reduce-Eta" $ nf graphTest gauEta
      -- -- , bench "gaussian  HHI-Reduce"       $ nf reducerTest gau
      -- -- , bench "gaussian  HHI-Eta"          $ nf reducerTest gauEta
      -- -- , bench "gaussian  HHI-Bulk"         $ nf reducerTest gauBulk
      -- -- , bench "gaussian  HHI-Bulk-Log"     $ nf reducerTestLog gauBulk
      -- -- , bench "gaussian  Native"           $ nf gaussianSum 100
      -- , bench "tak       Graph-Reduce"     $ nf graphTest tak
      --, bench "tak       Graph-Reduce-Eta" $ nf graphTest takEta
      -- , bench "tak       Graph-Reduce-Lin" $ nf graphTest takBulkLinear
      -- , bench "tak       Graph-Reduce-Log" $ nf graphTest takBulkLog
      -- , bench "tak       HHI-Reduce"       $ nf reducerTest tak
      , bench "tak       HHI-Eta"          $ nf reducerTest takEta
      -- , bench "tak       HHI-Bulk"         $ nf reducerTest takBulk
      -- , bench "tak       HHI-Bulk-Log"     $ nf reducerTestLog takBulk
      -- , bench "tak       HHI-Break-Bulk"   $ nf reducerTest takBulkLinear
      -- , bench "tak       HHI-Break-Log"    $ nf reducerTestLog takBulkLog
      , bench "tak       MicroHs"          $ nfIO (run mhsContext mhsTakEta)
      , bench "tak       Native"           $ nf tak1 (18,6,3) 
      ]
  closeMhsContext mhsContext
  putStrLn "Benchmarks completed."    
  return ()


fact :: Int -> Int
fact = fix (\f n -> if n == 0 then 1 else n * f (n-1))

fibo :: Int -> Int
fibo  = fix (\f n -> if n <= 1 then 1 else f (n-1) + f (n - 2))

ack_2 :: Int -> Int
ack_2 = ack 2

ack_3 :: Int -> Int
ack_3 = ack 3

ack :: Int -> Int -> Int
ack  = fix (\f n m ->
  if n == 0
    then m + 1
    else (if m == 0
      then f (n-1) 1
      else f (n-1) (f n (m-1))))

gaussianSum :: Int -> Int
gaussianSum  = fix (\f n -> if n == 0 then 0 else n + f (n-1))


tak_18_6 :: Int -> Int
tak_18_6 = takN 18 6

takN :: Int -> Int -> Int -> Int
takN  = fix (\f x y z -> (if y >= x then z else f (f (x-1) y z) (f (y-1) z x) (f (z-1) x y )))

tak1 (x,y,z) = takN x y z