module GraphReductionSpec where


import Parser
import LambdaToSKI
import Kiselyov
import GraphReduction
import Data.Maybe (fromJust)

import           Test.QuickCheck
import           Test.Hspec
import Control.Monad.ST (runST, ST)
import Data.STRef
import TestSources

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Classic GraphReduction with STRef" $ do
    it "computes factorial" $
      verify factorial
    it "computes fibonacci" $
      verify fibonacci
    it "computes gaussian sum" $
      verify gaussian
    it "computes ackermann function"  $
      verify ackermann
    it "computes tak " $
      verify tak

verify :: SourceCode -> IO ()
verify source = do
  runTest source
  runKiselyov source

getInt :: Expr -> Integer 
getInt (Int i) = i
getInt _ = error "not an int"

runKiselyov :: SourceCode -> IO ()
runKiselyov src = do
  let pEnv = parseEnvironment src
      expr = compileEta pEnv
      expected = show $ getInt $ fromJust (lookup "expected" pEnv)
      graph = allocate expr
      result = reduceGraph graph
      actual = runST $ printGraph result
  --print expr
  actual `shouldBe` expected

runTest :: SourceCode -> IO ()
runTest src = do
  let pEnv = parseEnvironment src
      expr = compile pEnv abstractToSKI
      graph = allocate expr
      expected = show $ getInt $ fromJust (lookup "expected" pEnv)
      result = reduceGraph graph
      actual = runST $ printGraph result
  actual `shouldBe` expected 

reduceGraph :: ST s (STRef s (Graph s)) -> ST s (STRef s (Graph s))
reduceGraph graph = do
  gP <- graph
  normalForm gP

printGraph :: ST s (STRef s (Graph s)) -> ST s String
printGraph graph = do
  gP <- graph
  toString gP  
