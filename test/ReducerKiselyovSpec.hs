module ReducerKiselyovSpec where

import HhiReducer
import Parser
import CLTerm
import Kiselyov
import Data.Maybe (fromJust)

import           Test.QuickCheck
import           Test.Hspec
import TestSources

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "hhi inspired Reducer (Kiselyov Abstraction)" $ do
    it "computes factorial Opt Eta" $
      verify factorial compileEta
    it "computes fibonacci Opt Eta" $
      verify fibonacci compileEta
    it "computes gaussian sum Opt Eta" $
      verify gaussian compileEta
    it "computes ackermann function Opt Eta"  $
      verify ackermann compileEta
    it "computes tak Opt Eta" $
      verify tak compileEta
    it "computes factorial BulkOpt" $
      verify factorial compileBulk
    it "computes fibonacci BulkOpt" $
      verify fibonacci compileBulk
    it "computes gaussian sum BulkOpt" $
      verify gaussian compileBulk
    it "computes ackermann function BulkOpt"  $
      verify ackermann compileBulk
    it "computes tak BulkOpt" $
      verify tak compileBulk

verify :: SourceCode -> (Environment -> CL) -> IO ()
verify src compileFun = do
  src `shouldSatisfy` runTest compileFun

runTest :: (Environment -> CL) -> SourceCode -> Bool
runTest compileFun src =
  let pEnv = parseEnvironment src
      aExp = compileFun pEnv
      tExp = translate aExp
      expected = translate $ toCL $ fromJust (lookup "expected" pEnv)
      actual = link primitives tExp
      actual' = transLink primitives aExp
  in    show expected == show actual 
     && show expected == show actual'
