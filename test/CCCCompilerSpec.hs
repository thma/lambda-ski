module CCCCompilerSpec where

import           Control.Exception (evaluate)
import           Data.List         (isInfixOf)
import           Data.Maybe        (fromJust)
import           Test.Hspec

import           CCC.Compiler
import           CCC.CatExpr   (CatExpr)
import           CCC.Interpreter (interp)
import           Parser         (Expr (..), parseEnvironment)
import           TestSources    (ackermann, cccAlias, cccConst, cccIdentity,
                                 cccLiteral, factorial, fibonacci, gaussian,
                                 tak)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CCC.Compiler compileNumExpr" $ do
    it "compiles an integer literal to a constant morphism" $ do
      let morph :: CatExpr () Integer
          morph = compileNumExpr [] (Int 5)
      interp morph () `shouldBe` 5

    it "compiles variable lookup to a constant morphism" $ do
      let morph :: CatExpr () Integer
          morph = compileNumExpr [("n", Int 13)] (Var "n")
      interp morph () `shouldBe` 13

    it "rejects non-numeric compilation results" $ do
      evaluate (interp (compileNumExpr [] (Lam "x" (Var "x")) :: CatExpr () Integer) ())
        `shouldThrow` anyErrorCall

    it "compiles unary y-recursion structurally using Fix" $ do
      let expr = App (App (Var "y") (Lam "f" (Lam "n"
                  (App (App (App (Var "if") (App (Var "is0") (Var "n")))
                       (Int 1))
                       (App (App (Var "*") (Var "n"))
                            (App (Var "f") (App (Var "sub1") (Var "n"))))))))
                 (Int 5)
          morph = compileNumExpr [] expr :: CatExpr () Integer
      show morph `shouldSatisfy` isInfixOf "Fix"
      interp morph () `shouldBe` 120

    it "compiles non-unary y-recursion structurally" $ do
      let expr = App (App (App (Var "y") (Lam "f" (Lam "x" (Lam "y" (Var "x"))))) (Int 1)) (Int 2)
          morph = compileNumExpr [] expr :: CatExpr () Integer
      show morph `shouldSatisfy` isInfixOf "Fix"
      interp morph () `shouldBe` 1

    it "compiles y-recursion structurally beyond arity 3" $ do
      let expr = App (App (App (App (App (Var "y")
                    (Lam "f" (Lam "a" (Lam "b" (Lam "c" (Lam "d" (Var "c")))))))
                    (Int 1)) (Int 2)) (Int 3)) (Int 4)
          morph = compileNumExpr [] expr :: CatExpr () Integer
      show morph `shouldSatisfy` isInfixOf "Fix"
      interp morph () `shouldBe` 3

    it "compiles recursion structurally via fix alias" $ do
      let expr = App (App (Var "fix") (Lam "f" (Lam "n"
                  (App (App (App (Var "if") (App (Var "is0") (Var "n")))
                       (Int 1))
                       (App (App (Var "*") (Var "n"))
                            (App (Var "f") (App (Var "sub1") (Var "n"))))))))
                 (Int 5)
          morph = compileNumExpr [] expr :: CatExpr () Integer
      show morph `shouldSatisfy` isInfixOf "Fix"
      interp morph () `shouldBe` 120

    it "compiles recursion structurally via environment alias" $ do
      let expr = App (App (Var "myFix") (Lam "f" (Lam "n"
                  (App (App (App (Var "if") (App (Var "is0") (Var "n")))
                       (Int 1))
                       (App (App (Var "*") (Var "n"))
                            (App (Var "f") (App (Var "sub1") (Var "n"))))))))
                 (Int 5)
          env = [("myFix", Var "y")]
          morph = compileNumExpr env expr :: CatExpr () Integer
      show morph `shouldSatisfy` isInfixOf "Fix"
      interp morph () `shouldBe` 120

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

  describe "CCC.Compiler compileNumExprNaive" $ do
    it "compiles an integer literal" $ do
      let morph :: CatExpr () Integer
          morph = compileNumExprNaive [] (Int 5)
      interp morph () `shouldBe` 5

    it "compiles variable lookup" $ do
      let morph :: CatExpr () Integer
          morph = compileNumExprNaive [("n", Int 13)] (Var "n")
      interp morph () `shouldBe` 13

    it "compiles unary y-recursion structurally using Fix" $ do
      let expr = App (App (Var "y") (Lam "f" (Lam "n"
                  (App (App (App (Var "if") (App (Var "is0") (Var "n")))
                       (Int 1))
                       (App (App (Var "*") (Var "n"))
                            (App (Var "f") (App (Var "sub1") (Var "n"))))))))
                 (Int 5)
          morph = compileNumExprNaive [] expr :: CatExpr () Integer
      show morph `shouldSatisfy` isInfixOf "Fix"
      interp morph () `shouldBe` 120

    it "produces same results as NBE for all TestSources programs" $ do
      mapM_ verifyNaiveMatchesNBE [cccLiteral, cccAlias, cccIdentity, cccConst,
                                    factorial, fibonacci, gaussian, ackermann, tak]

verifyNaiveMatchesNBE :: String -> Expectation
verifyNaiveMatchesNBE source = do
  let env = parseEnvironment source
      mainExpr = fromJust (lookup "main" env)
      nbeMorph :: CatExpr () Integer
      nbeMorph = compileNumExpr env mainExpr
      naiveMorph :: CatExpr () Integer
      naiveMorph = compileNumExprNaive env mainExpr
  interp naiveMorph () `shouldBe` interp nbeMorph ()

verifyMainMatchesExpected :: String -> Expectation
verifyMainMatchesExpected source = do
  let env = parseEnvironment source
  lookup "main" env `shouldSatisfy` (/= Nothing)
  lookup "expected" env `shouldSatisfy` (/= Nothing)

  let mainExpr = fromJust (lookup "main" env)
      expectedExpr = fromJust (lookup "expected" env)
      mainMorph :: CatExpr () Integer
      mainMorph = compileNumExpr env mainExpr
      expectedMorph :: CatExpr () Integer
      expectedMorph = compileNumExpr env expectedExpr

  interp mainMorph () `shouldBe` interp expectedMorph ()
