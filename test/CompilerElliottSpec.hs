module CompilerElliottSpec where

import           Data.Maybe (fromJust)
import           Test.Hspec

import           CCC.CompilerElliott (Cat (..), compile, absCCC, eval, evalTop)
import           Parser              (Expr (..), parseEnvironment)
import           TestSources         (ackermann, cccAlias, cccConst, cccIdentity,
                                      cccLiteral, factorial, fibonacci, gaussian,
                                      tak)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "absCCC core rules" $ do
    it "[x] x = snd" $
      absCCC "x" (CVar "x") `shouldBe` CSnd

    it "[x] y = y . fst (x not free)" $
      absCCC "x" (CVar "y") `shouldBe` CComp (CVar "y") CFst

    it "[x] (apply . <f, g>) = apply . <[x] f, [x] g>" $
      absCCC "x" (CComp CApply (CFan (CVar "x") (CVar "x")))
        `shouldBe` CComp CApply (CFan CSnd CSnd)

  describe "compile + eval" $ do
    it "compiles and evaluates an integer literal" $
      evalTop (compile [] (Int 42)) `shouldBe` 42

    it "compiles and evaluates a variable lookup" $
      evalTop (compile [("n", Int 13)] (Var "n")) `shouldBe` 13

    it "compiles and evaluates identity application" $
      evalTop (compile [] (App (Lam "x" (Var "x")) (Int 7))) `shouldBe` 7

    it "compiles and evaluates const function" $
      evalTop (compile [] (App (App (Lam "x" (Lam "y" (Var "x"))) (Int 3)) (Int 9)))
        `shouldBe` 3

    it "compiles and evaluates addition" $
      evalTop (compile [] (App (App (Var "+") (Int 3)) (Int 5))) `shouldBe` 8

    it "compiles and evaluates subtraction" $
      evalTop (compile [] (App (App (Var "-") (Int 10)) (Int 3))) `shouldBe` 7

    it "compiles and evaluates multiplication" $
      evalTop (compile [] (App (App (Var "*") (Int 4)) (Int 5))) `shouldBe` 20

    it "compiles and evaluates sub1" $
      evalTop (compile [] (App (Var "sub1") (Int 5))) `shouldBe` 4

    it "compiles and evaluates is0 true case" $
      evalTop (compile [] (App (App (App (Var "if") (App (Var "is0") (Int 0))) (Int 1)) (Int 2)))
        `shouldBe` 1

    it "compiles and evaluates is0 false case" $
      evalTop (compile [] (App (App (App (Var "if") (App (Var "is0") (Int 5))) (Int 1)) (Int 2)))
        `shouldBe` 2

  describe "fixpoint (y combinator)" $ do
    it "compiles and evaluates factorial 5" $ do
      let expr = App (App (Var "y") (Lam "f" (Lam "n"
                  (App (App (App (Var "if") (App (Var "is0") (Var "n")))
                       (Int 1))
                       (App (App (Var "*") (Var "n"))
                            (App (Var "f") (App (Var "sub1") (Var "n"))))))))
                 (Int 5)
      evalTop (compile [] expr) `shouldBe` 120

    it "compiles and evaluates via fix alias" $ do
      let expr = App (App (Var "fix") (Lam "f" (Lam "n"
                  (App (App (App (Var "if") (App (Var "is0") (Var "n")))
                       (Int 1))
                       (App (App (Var "*") (Var "n"))
                            (App (Var "f") (App (Var "sub1") (Var "n"))))))))
                 (Int 5)
      evalTop (compile [] expr) `shouldBe` 120

    it "compiles and evaluates multi-arg recursion" $ do
      let expr = App (App (App (Var "y") (Lam "f" (Lam "x" (Lam "y" (Var "x"))))) (Int 1)) (Int 2)
      evalTop (compile [] expr) `shouldBe` 1

  describe "integration with TestSources" $ do
    it "evaluates cccLiteral" $
      verifyMainMatchesExpected cccLiteral

    it "evaluates cccAlias" $
      verifyMainMatchesExpected cccAlias

    it "evaluates cccIdentity" $
      verifyMainMatchesExpected cccIdentity

    it "evaluates cccConst" $
      verifyMainMatchesExpected cccConst

    it "evaluates factorial" $
      verifyMainMatchesExpected factorial

    it "evaluates fibonacci" $
      verifyMainMatchesExpected fibonacci

    it "evaluates gaussian" $
      verifyMainMatchesExpected gaussian

    it "evaluates ackermann" $
      verifyMainMatchesExpected ackermann

    it "evaluates tak" $
      verifyMainMatchesExpected tak

verifyMainMatchesExpected :: String -> Expectation
verifyMainMatchesExpected source = do
  let env = parseEnvironment source
      mainExpr     = fromJust (lookup "main" env)
      expectedExpr = fromJust (lookup "expected" env)
  evalTop (compile env mainExpr) `shouldBe` evalTop (compile env expectedExpr)
