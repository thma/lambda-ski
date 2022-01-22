module Reducer where

import Parser ( parseEnvironment, Environment, Expr(..) )
import LambdaToSKI
import Data.Maybe (fromJust)
import Data.Bifunctor ( Bifunctor(second) )
import Control.Monad.Fix (fix)
import Data.Type.Coercion (trans)

type Name = String

data CExpr
  = CVar Name
  | CApp CExpr CExpr
  | CLam (CExpr -> CExpr)
  | CInt Integer

instance Show CExpr where
  show (CVar n)   = n
  show (CApp a b) = "(" ++ show a ++ " " ++ show b ++ ")"
  show (CLam f)   = "<function>"
  show (CInt i)   = show i


compile' :: Environment -> Expr -> CExpr
compile' env (fun :@ arg)   = CApp (compile' env fun) (compile' env arg)
compile' env (Lam x body)   = abstract x (compile' env body)
compile' env (Int k)        = CInt k
compile' env (Var n)
  | Just t <- lookup n env = compile' env t
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

translate :: Expr -> CExpr
translate (fun :@ arg)   = CApp (translate fun) (translate arg)
translate (Int k)        = CInt k
translate (Var c)        = CVar c
translate lam            = error $ "lambdas should be abstracted already " ++ show lam

infixl 0 !
(!) :: CExpr -> CExpr -> CExpr
(CLam f) ! x = f x
-- (CApp f x) ! y = (f ! x) ! y
-- (CVar n) ! x = error $ n ++ " was not properly compiled"
-- (CInt i) ! x = CInt i
e ! x = error $ "can't apply " ++ show e ++ " to " ++ show x

primitives :: [(String, CExpr)]
primitives = let (-->) = (,) in
  [ "i"   --> CLam id
  , "k"   --> CLam (CLam . const)
  , "s"   --> CLam (\f -> CLam $ \g -> CLam $ \x -> f!x!(g!x))
  , "b"   --> CLam (\f -> CLam $ \g -> CLam $ \x -> f!(g!x))
  , "c"   --> CLam (\f -> CLam $ \g -> CLam $ \x -> f!x!g)
  , "if"   --> CLam (\(CInt cond) -> CLam $ \tr -> CLam $ \fl -> if cond == 1 then tr else fl)
  , "y"    --> CLam (\(CLam f) -> fix f)
  , "+"    --> arith (+)
  , "sub"  --> arith (-)
  , "sub1" --> CLam sub1
  , "*"    --> arith (*)
  , "eql"  --> arith eql
  , "geq"  --> arith geq
  , "is0"  --> CLam isZero
  ]

arith :: (Integer -> Integer -> Integer) -> CExpr
arith op = CLam $ \(CInt a) -> CLam $ \(CInt b) -> CInt (op a b)

eql :: (Eq a, Num p) => a -> a -> p
eql n m = if n == m then 1 else 0

geq :: (Ord a, Num p) => a -> a -> p
geq n m = if n >= m then 1 else 0

leq :: (Ord a, Num p) => a -> a -> p
leq n m = if n <= m then 1 else 0

gre :: (Ord a, Num p) => a -> a -> p
gre n m = if n > m then 1 else 0

le :: (Ord a, Num p) => a -> a -> p
le n m = if n < m then 1 else 0

sub1 :: CExpr -> CExpr
sub1 (CInt n) = CInt $ n -1
sub1 x        = error $ show x ++ " is not a number"

isZero :: CExpr -> CExpr
isZero (CInt n) = if n == 0 then CInt 1 else CInt 0
isZero _        = CInt 0

type GlobalEnv = [(String,CExpr)]

compileEnv :: Environment -> GlobalEnv
compileEnv env = map (second $ compile' env) env

link :: GlobalEnv -> CExpr -> CExpr
link bs (CApp fun arg) = link bs fun ! link bs arg
link bs (CVar n)       = case lookup n bs of
  Nothing -> error $ n ++ " is not defined"
  Just ce -> ce
link _ e               = e


getMain :: [([Char], a)] -> a
getMain env = fromJust $ lookup "main" env

eval :: GlobalEnv -> String -> CExpr
eval env src =
  let pEnv = parseEnvironment src
  in  link env $ compile' pEnv $ getMain pEnv

eval' :: GlobalEnv -> String -> CExpr
eval' globals src =
  let pEnv = parseEnvironment src
      aExp = compile pEnv abstractToSKI
      tExp = translate aExp
  in  link globals tExp  


evalFile' :: FilePath -> IO CExpr
evalFile' file = do
  src <- readFile file
  let pEnv = parseEnvironment src
      aExp = compile pEnv abstractSimple
      tExp = translate aExp  

  putStrLn "compiled to SICKBY:"
  print aExp
  putStrLn "compiled to CExpr"
  print tExp

  return $ link primitives tExp

abstractSimple :: Environment -> Expr -> Expr
abstractSimple env = ropt . babs0 env


evalFile :: FilePath -> IO CExpr
evalFile file = do
  src <- readFile file
  return $ eval' primitives src

test = do
  let testCases = [
         "factorial.ths"
       , "fibonacci.ths"
       , "tak.ths"
       , "ackermann.ths"
       , "gaussian.ths"
       ]
  mapM_ (\tc -> putStrLn tc >> evalFile' ("test/" ++ tc) >>= print) testCases


