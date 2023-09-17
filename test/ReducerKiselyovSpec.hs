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

    it "computes factorial BulkOpt Linear" $
      verify factorial compileBulk
    it "computes fibonacci BulkOpt Linear" $
      verify fibonacci compileBulk
    it "computes gaussian sum BulkOpt Linear" $
      verify gaussian compileBulk
    it "computes ackermann function BulkOpt Linear"  $
      verify ackermann compileBulk
    it "computes tak BulkOpt Linear" $
      verify tak compileBulk

    it "computes factorial BulkOpt Log" $
      verifyLog factorial compileBulk
    it "computes fibonacci BulkOpt Log" $
      verifyLog fibonacci compileBulk
    it "computes gaussian sum BulkOpt Log" $
      verifyLog gaussian compileBulk
    it "computes ackermann function BulkOpt Log"  $
      verifyLog ackermann compileBulk
    it "computes tak BulkOpt Log" $
      verifyLog tak compileBulk      

verify :: SourceCode -> (Environment -> CL) -> IO ()
verify src compileFun = do
  src `shouldSatisfy` runTest compileFun (link, transLink)

verifyLog :: SourceCode -> (Environment -> CL) -> IO ()
verifyLog src compileFun = do
  src `shouldSatisfy` runTest compileFun (linkLog, transLinkLog)

runTest :: (Environment -> CL) -> (CombinatorDefinitions -> CExpr -> CExpr,     CombinatorDefinitions -> CL -> CExpr) -> [Char] -> Bool
runTest compileFun (linkFun, transLinkFun) src =
  let pEnv = parseEnvironment src
      aExp = compileFun pEnv
      tExp = translate aExp
      expected = translate $ toCL $ fromJust (lookup "expected" pEnv)
      actual = linkFun primitives tExp
      actual' = transLinkFun primitives aExp
  in    show expected == show actual 
     && show expected == show actual'
