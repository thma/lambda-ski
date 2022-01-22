module GeneratorBench where

import Criterion.Main
import Parser
import LambdaToSKI
import GraphReduction hiding (nf)
import Data.Maybe (fromJust)
import Data.STRef
import Control.Monad.ST

type SourceCode = String

loadTestCase :: String -> IO SourceCode
loadTestCase name = readFile $ "test/" ++ name ++ ".ths"

getInt :: Expr -> Integer 
getInt (Int i) = i
getInt _ = error "not an int"

--runTest :: SourceCode -> Bool
runTest src =
  let pEnv = parseEnvironment src
      expr = compile pEnv abstractSimple
      graph = allocate expr
      expected = show $ getInt $ fromJust (lookup "expected" pEnv)
      result = reduceGraph graph
      actual = runST $ printGraph result
  in  actual
  --print expected
  --print actual

printGraph :: ST s (STRef s (Graph s)) -> ST s String
printGraph graph = do
  gP <- graph
  toString gP

reduceGraph :: ST s (STRef s (Graph s)) -> ST s (STRef s (Graph s))
reduceGraph graph = do
  gP <- graph
  normalForm gP



generatorBench = do
  src <- loadTestCase "factorial"

  defaultMain [
        bench "compute factorial"    $ nf runTest src

      ]
  return ()
