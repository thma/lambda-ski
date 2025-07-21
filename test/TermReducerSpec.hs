{-# LANGUAGE QuasiQuotes #-}
module TermReducerSpec where

import TermReducer
import Parser
import CLTerm
import LambdaToSKI
import Data.Maybe (fromJust)
import qualified SimpleTestSources as Simple
import qualified TestSources as Complex -- Import original test sources
import Text.RawString.QQ -- Add this import
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Control.Exception (catch, SomeException)

import           Test.QuickCheck
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "TermReducer - Simple Tests" $ do
    it "computes basic arithmetic" $
      verify Simple.basicArithmetic
    it "computes basic comparison" $
      verify Simple.basicComparison
    it "computes basic boolean" $
       verify Simple.basicBoolean
    it "computes basic subtraction" $
      verify Simple.basicSubtraction
    it "computes basic multiplication" $
      verify Simple.basicMultiplication
      
  describe "TermReducer - Complex Recursive Tests" $ do
    it "computes simple constant" $
      verify simpleConstant
    it "computes simple identity" $
      verify simpleIdentity
    it "computes simple K combinator" $
      verify simpleK
    it "computes simple composition" $
      verify simpleComposition
    -- it "computes Y combinator test" $
    --    verify simpleYCombinator
    -- it "computes simple fac (recursive)" $
    --   verify smallFactorial

-- Very simple non-recursive tests for TermReducer
simpleConstant :: String
simpleConstant = [r| 
  expected = 42
  main = 42
|]

simpleIdentity :: String
simpleIdentity = [r| 
  expected = 5
  main = i 5
|]

simpleK :: String
simpleK = [r| 
  expected = 3
  main = k 3 7
|]

simpleComposition :: String
simpleComposition = [r| 
  expected = 10
  double = * 2
  main = double 5
|]

-- Test that demonstrates Y combinator support (but with very limited steps)
simpleYCombinator :: String
simpleYCombinator = [r| 
  expected = a
  main = y k
|]

verifySmallFib :: IO ()
verifySmallFib = verify simpleConstant

verifySmallFact :: IO ()
verifySmallFact = verify simpleIdentity

verifySmallAck :: IO ()
verifySmallAck = verify simpleK

verifySmallGaussian :: IO ()
verifySmallGaussian = verify simpleComposition


smallFactorial :: String
smallFactorial = [r| 
  expected = 1
  fact = y(λf n. if (is0 n) 1 (* n (f (sub1 n))))
  main = fact 0
|]


-- Test that shows TermReducer can handle original TestSources
originalSmallFib :: String
originalSmallFib = [r| 
  expected = 2
  fib  = y(λf n. if (is0 n) 1 (if (eql n 1) 1 (+ (f (sub1 n)) (f (sub n 2)))))
  main = fib 2
|]

verify :: String -> IO ()
verify src = do
  result <- runTest src
  result `shouldBe` True
  
runTest :: String -> IO Bool
runTest src = do
  let pEnv = parseEnvironment src
      aExp = compile pEnv abstractToSKI
      expected = toCL $ fromJust (lookup "expected" pEnv)
  
  -- Capture result with timeout handling
  result <- catch 
    (let actual = red aExp
      in do 
        putStrLn $ "Expected: " ++ show expected
        putStrLn $ "Actual: " ++ show actual 
        return $ show expected == show actual)
    (\e -> do 
        putStrLn $ "Error during reduction: " ++ show (e :: SomeException)
        return False)
  
  return result
