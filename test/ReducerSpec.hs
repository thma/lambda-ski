module ReducerSpec where

import HhiReducer hiding (evalFile)
import Parser
import LambdaToSKI
import Data.Maybe (fromJust)

import           Test.QuickCheck
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "hhi inspired Reducer " $ do
    it "computes factorial" $
      verify "factorial"
    it "computes fibonacci" $
      verify "fibonacci"
    it "computes gaussian sum" $
      verify "gaussian"
    it "computes ackermann function"  $
      verify "ackermann"
    it "computes tak " $
      verify "tak"


verify :: String -> IO ()
verify name = do
  tc <- loadTestCase name
  tc `shouldSatisfy` runTest 

type SourceCode = String

loadTestCase :: String -> IO SourceCode
loadTestCase name = readFile $ "test/" ++ name ++ ".ths"

runTest :: SourceCode -> Bool
runTest src =
  let pEnv = parseEnvironment src
      aExp = compile pEnv abstractSimple
      tExp = translate aExp
      expected = translate $ fromJust (lookup "expected" pEnv)
      actual = link primitives tExp
  in show expected == show actual
