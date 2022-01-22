module GraphReductionSpec where


import Parser
import LambdaToSKI
import GraphReduction
import Data.Maybe (fromJust)

import           Test.QuickCheck
import           Test.Hspec
import Control.Monad.ST (runST, ST)
import Data.STRef

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Classic GraphReduction with STRef" $ do
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
  let (expected, actual) = runTest tc
  actual `shouldBe` expected 

type SourceCode = String

loadTestCase :: String -> IO SourceCode
loadTestCase name = readFile $ "test/" ++ name ++ ".ths"

getInt :: Expr -> Integer 
getInt (Int i) = i
getInt _ = error "not an int"

runTest :: SourceCode -> (String, String)
runTest src =
  let pEnv = parseEnvironment src
      expr = compile pEnv abstractSimple
      graph = allocate expr
      expected = show $ getInt $ fromJust (lookup "expected" pEnv)
      result = reduceGraph graph
      actual = runST $ printGraph result
  in (show expected, show actual)

reduceGraph :: ST s (STRef s (Graph s)) -> ST s (STRef s (Graph s))
reduceGraph graph = do
  gP <- graph
  normalForm gP

printGraph :: ST s (STRef s (Graph s)) -> ST s String
printGraph graph = do
  gP <- graph
  toString gP  
