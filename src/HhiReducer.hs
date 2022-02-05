module HhiReducer where

import Parser ( Expr(..) ) 
import Control.Monad.Fix (fix)
import LambdaToSKI (Combinator (..), fromString)
import Data.Maybe (fromJust)

-- | a compiled expression
data CExpr = 
    CComb Combinator
  | CApp CExpr CExpr
  | CFun (CExpr -> CExpr)
  | CInt Integer

instance Show CExpr where
  show (CComb k)  = show k
  show (CApp a b) = "(" ++ show a ++ " " ++ show b ++ ")"
  show (CFun _f)  = "<function>"
  show (CInt i)   = show i

-- | translating a lambda expression into a compiled expression
translate :: Expr -> CExpr
translate (fun :@ arg)   = CApp (translate fun) (translate arg)
translate (Int k)        = CInt k
translate (Var c)        = CComb (fromString c)
translate lam@(Lam _ _)  = error $ "lambdas should be abstracted already " ++ show lam

-- | apply a CExpr of shape (CFun f) to argument x by evaluating (f x)
infixl 0 !
(!) :: CExpr -> CExpr -> CExpr
(CFun f) ! x = f x
{-# INLINE (!) #-}

type GlobalEnv = [(Combinator,CExpr)]

-- | "link" a compiled expression into Haskell native functions.
--   application terms will be transformed into real (!) applications
--   combinator symbols will be replaced by their actual function definition
link :: GlobalEnv -> CExpr -> CExpr
link globals (CApp fun arg) = link globals fun ! link globals arg
link globals (CComb comb)   = fromJust $ lookup comb globals
link _globals expr          = expr

-- | translate and link in one go
transLink :: GlobalEnv -> Expr -> CExpr
transLink globals (fun :@ arg)  = transLink globals fun ! transLink globals arg
transLink _globals (Int k)      = CInt k
transLink globals (Var c)       = fromJust $ lookup (fromString c) globals
transLink _globals l@(Lam _ _)  = error $ "lambdas should be abstracted already " ++ show l


primitives :: GlobalEnv
primitives = let (-->) = (,) in
  [ I      --> CFun id
  , K      --> CFun (CFun . const)
  , S      --> CFun (\f -> CFun $ \g -> CFun $ \x -> f!x!(g!x))
  , B      --> CFun (\f -> CFun $ \g -> CFun $ \x -> f!(g!x))
  , C      --> CFun (\f -> CFun $ \g -> CFun $ \x -> f!x!g)
  , IF     --> CFun (\(CInt cond) -> CFun $ \thenExp -> CFun $ \elseExp -> if cond == 1 then thenExp else elseExp)
  , Y      --> CFun (\(CFun f) -> fix f)
  , ADD    --> arith (+)
  , SUB    --> arith (-)
  , SUB1   --> CFun sub1
  , MUL    --> arith (*)
  , EQL    --> arith eql
  , GEQ    --> arith geq
  , ZEROP  --> CFun isZero
  ]

arith :: (Integer -> Integer -> Integer) -> CExpr
arith op = CFun $ \(CInt a) -> CFun $ \(CInt b) -> CInt (op a b)

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