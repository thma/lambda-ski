module ReducerSpec where

import HhiReducer
import Parser
import LambdaToSKI
import Data.Maybe (fromJust)
import TestSources
    ( ackermann, factorial, fibonacci, gaussian, tak )

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
verify tc = do
  tc `shouldSatisfy` runTest 

type SourceCode = String

loadTestCase :: String -> IO SourceCode
loadTestCase name = readFile $ "test/" ++ name ++ ".ths"

runTest :: SourceCode -> Bool
runTest src =
  let pEnv = parseEnvironment src
      aExp = compile pEnv abstractToSKI
      tExp = translate aExp
      expected = translate $ fromJust (lookup "expected" pEnv)
      actual = link primitives tExp
  in show expected == show actual
