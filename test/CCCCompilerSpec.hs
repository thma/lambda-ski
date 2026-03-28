module CCCCompilerSpec where

import           Control.Exception (evaluate)
import           Data.List         (isInfixOf)
import           Data.Maybe        (fromJust)
import           Test.Hspec

import           CCC.Compiler
import           CCC.FreeCat   (FreeCat)
import           CCC.Interpreter (interp)
import           Parser         (Expr (..), parseEnvironment)
import           TestSources    (ackermann, cccAlias, cccConst, cccIdentity,
                                 cccLiteral, factorial, fibonacci, gaussian,
                                 tak)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CCC.Compiler evalExpr" $ do
    it "evaluates integer literals" $ do
      show (evalExpr [] (Int 42)) `shouldBe` "42"

    it "resolves variables through the environment" $ do
      show (evalExpr [("x", Int 7)] (Var "x")) `shouldBe` "7"

    it "evaluates lambda application" $ do
      let expr = App (Lam "x" (Var "x")) (Int 9)
      show (evalExpr [] expr) `shouldBe` "9"

    it "throws for unbound variables" $ do
      evaluate (evalExpr [] (Var "missing")) `shouldThrow` anyErrorCall

  describe "CCC.Compiler compileNumExpr" $ do
    it "compiles an integer literal to a constant morphism" $ do
      let morph :: FreeCat () Integer
          morph = compileNumExpr [] (Int 5)
      interp morph () `shouldBe` 5

    it "compiles variable lookup to a constant morphism" $ do
      let morph :: FreeCat () Integer
          morph = compileNumExpr [("n", Int 13)] (Var "n")
      interp morph () `shouldBe` 13

    it "rejects non-numeric compilation results" $ do
      evaluate (compileNumExpr [] (Lam "x" (Var "x")) :: FreeCat () Integer)
        `shouldThrow` anyErrorCall

  describe "CCC.Compiler environment helpers" $ do
    it "returns Right for numeric bindings" $ do
      case tryCompileVar [("k", Int 11)] "k" of
        Right morph -> interp morph () `shouldBe` 11
        Left err    -> expectationFailure ("unexpected error: " ++ err)

    it "returns Left for missing bindings" $ do
      let isMissingError (Left msg) = "not found" `isInfixOf` msg
          isMissingError _          = False
      tryCompileVar [] "k" `shouldSatisfy` isMissingError

    it "summarizes mixed environments" $ do
      let env =
            [ ("a", Int 1)
            , ("f", Lam "x" (Var "x"))
            ]
          (successes, failures) = compileNumericBindings env
      length successes `shouldBe` 1
      length failures `shouldBe` 1

    it "formats compileEnvironment entries" $ do
      let env =
            [ ("a", Int 3)
            , ("id", Lam "x" (Var "x"))
            ]
          out = compileEnvironment env
      lookup "a" out `shouldBe` Just "IntConst 3"
      lookup "id" out `shouldBe` Just "<lambda function>"

  describe "CCC.Compiler integration with TestSources" $ do
    it "parses and compiles a literal program" $ do
      verifyMainMatchesExpected cccLiteral

    it "parses and compiles an alias program" $ do
      verifyMainMatchesExpected cccAlias

    it "parses and compiles an identity application program" $ do
      verifyMainMatchesExpected cccIdentity

    it "parses and compiles a constant function program" $ do
      verifyMainMatchesExpected cccConst

    it "parses and compiles factorial" $ do
      verifyMainMatchesExpected factorial

    it "parses and compiles fibonacci" $ do
      verifyMainMatchesExpected fibonacci

    it "parses and compiles gaussian" $ do
      verifyMainMatchesExpected gaussian

    it "parses and compiles ackermann" $ do
      verifyMainMatchesExpected ackermann

    it "parses and compiles tak" $ do
      verifyMainMatchesExpected tak

verifyMainMatchesExpected :: String -> Expectation
verifyMainMatchesExpected source = do
  let env = parseEnvironment source
  lookup "main" env `shouldSatisfy` (/= Nothing)
  lookup "expected" env `shouldSatisfy` (/= Nothing)

  let mainExpr = fromJust (lookup "main" env)
      expectedExpr = fromJust (lookup "expected" env)
      mainMorph :: FreeCat () Integer
      mainMorph = compileNumExpr env mainExpr
      expectedMorph :: FreeCat () Integer
      expectedMorph = compileNumExpr env expectedExpr

  interp mainMorph () `shouldBe` interp expectedMorph ()
