module ReducerKiselyovSpec where

import HhiReducer
import Parser
import CLTerm
import Kiselyov
import Data.Maybe (fromJust)
import TestSources
    ( ackermann, factorial, fibonacci, gaussian, tak )

import           Test.QuickCheck
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "hhi inspired Reducer (Kiselyov compiler)" $ do
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
  showCode tc
  tc `shouldSatisfy` runTest 

type SourceCode = String

loadTestCase :: String -> IO SourceCode
loadTestCase name = readFile $ "test/" ++ name ++ ".ths"

showCode :: SourceCode -> IO ()
showCode src = do
  let pEnv = parseEnvironment src
      aExp = compileKi pEnv optEta
  putStrLn "The source: "
  putStrLn src
  putStrLn "The result of the kiselyov compiler:"
  print aExp
  
runTest :: SourceCode -> Bool
runTest src =
  let pEnv = parseEnvironment src
      aExp = compileKi pEnv optEta --compile pEnv abstractToSKI
      tExp = translate aExp
      expected = translate $ toCL $ fromJust (lookup "expected" pEnv)
      actual = link primitives tExp
  in show expected == show actual
