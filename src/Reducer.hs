module Reducer where

import Parser
import Data.Maybe (fromJust)
import Data.Bifunctor ( Bifunctor(second) )
import Control.Monad.Fix (fix)

type Name = String

data CExpr
  = CVar Name
  | CApp CExpr CExpr
  | CLam (CExpr -> CExpr)
  | CInt Integer

instance Show CExpr where
  show (CVar n)   = n
  show (CApp a b) = "(" ++ show a ++ " " ++ show b ++ ")"
  show (CLam f)   = "fn"
  show (CInt i)   = show i


compile :: Environment -> Expr -> CExpr
compile env (fun :@ arg)   = CApp (compile env fun) (compile env arg)
compile env (Lam x body)   = abstract x (compile env body)
compile env (Int k)        = CInt k
compile env (Var n)
  | Just t <- lookup n env = compile env t
  | otherwise              = CVar n

abstract :: Name -> CExpr -> CExpr
abstract x (CApp fun arg)    = combS (abstract x fun) (abstract x arg)
abstract x (CVar n) | x == n = combI
abstract _ k                 = combK k

combS :: CExpr -> CExpr -> CExpr
combS f = CApp (CApp (CVar "$S") f)

combK :: CExpr -> CExpr
combK = CApp (CVar "$K")

combI :: CExpr
combI = CVar "$I"

infixl 0 !
(!) :: CExpr -> CExpr -> CExpr
(CLam f) ! x = f x
-- (CApp f x) ! y = (f ! x) ! y
-- (CVar n) ! x = error $ n ++ " was not properly compiled"
-- (CInt i) ! x = CInt i
e ! x = error $ "can't apply " ++ show e ++ " to " ++ show x

primitives :: [(String, CExpr)]
primitives = let (-->) = (,) in
  [ "$I"   --> CLam id
  , "$K"   --> CLam (CLam . const)
  , "$S"   --> CLam (\f -> CLam $ \g -> CLam $ \x -> f!x!(g!x))
  , "$B"   --> CLam (\f -> CLam $ \g -> CLam $ \x -> f!(g!x))
  , "$C"   --> CLam (\f -> CLam $ \g -> CLam $ \x -> f!x!g)
  , "if"   --> CLam (\(CInt cond) -> CLam $ \tr -> CLam $ \fl -> if cond == 1 then tr else fl)
  , "y"    --> CLam (\(CLam f) -> fix f)
  , "+"    --> arith (+)
  , "sub"  --> arith (-)
  , "sub1" --> CLam sub1
  , "*"    --> arith (*)
  , "eql"  --> arith eql
  , "is0"  --> CLam isZero
  ]

arith :: (Integer -> Integer -> Integer) -> CExpr
arith op = CLam $ \(CInt a) -> CLam $ \(CInt b) -> CInt (op a b)

eql :: (Eq a, Num p) => a -> a -> p
eql n m = if n == m then 1 else 0

sub1 :: CExpr -> CExpr
sub1 (CInt n) = CInt $ n -1

isZero :: CExpr -> CExpr
isZero (CInt n) = if n == 0 then CInt 1 else CInt 0

type TermEnv = [(String,CExpr)]

compileEnv :: Environment -> TermEnv
compileEnv env = map (second $ compile env) env

link :: TermEnv -> CExpr -> CExpr
link bs (CApp fun arg) = link bs fun ! link bs arg
link bs (CVar n)       = case lookup n bs of
  Nothing -> error $ n ++ " is not defined"
  Just ce -> ce
link _ e               = e


getMain :: [([Char], a)] -> a
getMain env = fromJust $ lookup "main" env

eval :: TermEnv -> String -> CExpr
eval env src =
  let pEnv = parseEnvironment src
  in  link env $ compile pEnv $ getMain pEnv



--src = "main = (λx -> (+ 1 x)) 2"
src = "Y    = λf -> (λx -> x x)(λx -> f(x x)) \n"
  ++ "fact = Y(λf n. if (is0 n) 1 (* n (f (sub1 n)))) \n"
  ++ "main = fact 10000 \n"

-- src = "main = (\\x -> * x (add1 x)) 5 \n"
--   ++  "add1 = \\x -> + x 1"

test = do
  print $ eval primitives src

