module ReducerSpec where

import HhiReducer
import Parser
import CLTerm
import LambdaToSKI
import Data.Maybe (fromJust)
import TestSources

import           Test.QuickCheck
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "hhi inspired Reducer " $ do
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
verify src = do
  src `shouldSatisfy` runTest
  
runTest :: SourceCode -> Bool
runTest src =
  let pEnv = parseEnvironment src
      aExp = compile pEnv abstractToSKI
      tExp = translate aExp
      expected = translate $ toCL $ fromJust (lookup "expected" pEnv)
      actual = link primitives tExp
  in show expected == show actual
