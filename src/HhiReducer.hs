module HhiReducer where

import Parser ( Expr(..) ) 
import Control.Monad.Fix (fix)
import CLTerm 
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

-- | translating a CL term expression into a compiled expression
translate :: CL -> CExpr
translate (fun :@ arg)   = CApp (translate fun) (translate arg)
translate (INT k)        = CInt k
translate (Com c)        = CComb c
--translate lam@(Lam _ _)  = error $ "lambdas should be abstracted already " ++ show lam

-- | apply a CExpr of shape (CFun f) to argument x by evaluating (f x)
infixl 0 !
(!) :: CExpr -> CExpr -> CExpr
(CFun f) ! x = f x
x ! y = error $ "can't handle " ++ show x
{-# INLINE (!) #-}

-- | "link" a compiled expression into Haskell native functions.
--   application terms will be transformed into real (!) applications
--   combinator symbols will be replaced by their actual function definition
link :: CombinatorDefinitions -> CExpr -> CExpr
link definitions (CApp fun arg) = link definitions fun ! link definitions arg
link definitions (CComb comb)   = case lookup comb definitions of
  Nothing -> resolveBulk comb --error $ "unknown combinator " ++ show comb
  Just e  -> e
link _definitions expr          = expr

-- | translate and link in one go
transLink :: CombinatorDefinitions -> CL -> CExpr
transLink definitions (fun :@ arg)  = transLink definitions fun ! transLink definitions arg
transLink _definitions (INT k)      = CInt k
transLink definitions (Com c)       = case lookup c definitions of
  Nothing -> error $ "unknown combinator " ++ show c
  Just e  -> e

type CombinatorDefinitions = [(Combinator,CExpr)]

-- | the set of primary operations: combinators + basic arithmetic functions
primitives :: CombinatorDefinitions
primitives = let (-->) = (,) in
  [ I      --> CFun id
  , K      --> CFun (CFun . const)
  , S      --> comS --CFun (\f -> CFun $ \g -> CFun $ \x -> f!x!(g!x)) -- S F G X = F X (G X)
  , B      --> comB --CFun (\f -> CFun $ \g -> CFun $ \x -> f!(g!x))   -- B F G X = F (G X)
  , C      --> comC --CFun (\f -> CFun $ \g -> CFun $ \x -> f!x!g)     -- C F G X = F X G
  , R      --> CFun (\f -> CFun $ \g -> CFun $ \x -> g!x!f)     -- R F G X = G X F  
 -- , T      --> CFun (\x -> CFun $ \y -> x)                    -- T X Y = X
  , B'     --> comB' --CFun (\p -> CFun $ \q -> CFun $ \r -> CFun $ \s -> p!q!(r!s))      -- B' P Q R S = P Q (R S)
  , C'     --> comC' --CFun (\p -> CFun $ \q -> CFun $ \r -> CFun $ \s -> p!(q!s)!r)      -- C' P Q R S = P (Q S) R
  , S'     --> comS' --CFun (\p -> CFun $ \q -> CFun $ \r -> CFun $ \s -> p!(q!s)!(r!s))  -- S' P Q R S = P (Q S) (R S)
  , IF     --> CFun (\(CInt cond) -> CFun $ \thenExp -> CFun $ \elseExp -> if cond == 1 then thenExp else elseExp)
  , Y      --> CFun (\(CFun f) -> fix f)
  , ADD    --> arith (+)
  , SUB    --> arith (-)
  , SUB1   --> CFun sub1
  , MUL    --> arith (*)
  , EQL    --> arith eql
  , GEQ    --> arith geq
  , ZEROP  --> CFun isZero
  -- , S2     --> comS2
  -- , S3     --> comS3
  -- , S4     --> comS4
  -- , B2     --> comB2
  -- , B3     --> comB3
  -- , C2     --> comC2
  -- , C3     --> comC3
  ]


resolveBulk :: Combinator -> CExpr
resolveBulk (BulkCom "B" n) = iterate (comB' !) comB !! (n-1) 
resolveBulk (BulkCom "C" n) = iterate (comC' !) comC !! (n-1)
resolveBulk (BulkCom "S" n) = iterate (comS' !) comS !! (n-1) 
resolveBulk anyOther = error $ "not a known combinator: " ++ show anyOther

comS :: CExpr
comS = CFun (\f -> CFun $ \g -> CFun $ \x -> f!x!(g!x)) -- S F G X = F X (G X)

comS' :: CExpr
comS' = CFun (\p -> CFun $ \q -> CFun $ \r -> CFun $ \s -> p!(q!s)!(r!s))  -- S' P Q R S = P (Q S) (R S)

comS2 :: CExpr
comS2 = comS' ! comS

comS3 = comS' ! comS2
comS4 = comS' ! comS3

comB = CFun (\f -> CFun $ \g -> CFun $ \x -> f!(g!x))   -- B F G X = F (G X)
comB' = CFun (\p -> CFun $ \q -> CFun $ \r -> CFun $ \s -> p!q!(r!s))      -- B' P Q R S = P Q (R S)

comB2 = comB' ! comB
comB3 = comB' ! comB2

comC = CFun (\f -> CFun $ \g -> CFun $ \x -> f!x!g)     -- C F G X = F X G
comC' = CFun (\p -> CFun $ \q -> CFun $ \r -> CFun $ \s -> p!(q!s)!r)      -- C' P Q R S = P (Q S) R

comC2 = comC' ! comC
comC3 = comC' ! comC2


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