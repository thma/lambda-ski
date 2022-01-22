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

translate :: Expr -> CExpr
translate (fun :@ arg)   = CApp (translate fun) (translate arg)
translate (Int k)        = CInt k
translate (Var c)        = CVar c
translate lam            = error $ "lambdas should be abstracted already " ++ show lam

infixl 0 !
(!) :: CExpr -> CExpr -> CExpr
(CLam f) ! x = f x
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

link :: GlobalEnv -> CExpr -> CExpr
link bs (CApp fun arg) = link bs fun ! link bs arg
link bs (CVar n)       = case lookup n bs of
  Nothing -> error $ n ++ " is not defined"
  Just ce -> ce
link _ e               = e


eval :: GlobalEnv -> String -> CExpr
eval globals src =
  let pEnv = parseEnvironment src
      aExp = compile pEnv abstractToSKI
      tExp = translate aExp
  in  link globals tExp


evalFile :: FilePath -> IO CExpr
evalFile file = do
  src <- readFile file
  return $ eval primitives src

evalFile' :: FilePath -> IO CExpr
evalFile' file = do
  src <- readFile file
  let pEnv = parseEnvironment src
      aExp = compile pEnv abstractSimple
      tExp = translate aExp
      expected = translate $ fromJust (lookup "expected" pEnv)

  putStrLn "compiled to SICKBY:"
  print aExp
  putStrLn "compiled to CExpr"
  print tExp

  putStrLn "expected result:"
  print expected

  let actual = link primitives tExp
  putStrLn "actual result:"
  print actual

  if show expected == show actual then return actual else fail $ "test " ++ file ++ " failed."

test = do
  let testCases = [
         "factorial.ths"
       , "fibonacci.ths"
       , "tak.ths"
       , "ackermann.ths"
       , "gaussian.ths"
       ]
  mapM_ (\tc -> putStrLn tc >> evalFile' ("test/" ++ tc) >>= print) testCases


