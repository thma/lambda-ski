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

-- | apply a CExpr of shape (CFun f) to argument x by evaluating (f x)
infixl 0 !
(!) :: CExpr -> CExpr -> CExpr
(CFun f) ! x = f x
(CComb c) ! x = link primitives (CComb c) ! x
x ! y = error $ "can't handle " ++ show x
{-# INLINE (!) #-}

-- | "link" a compiled expression into Haskell native functions.
--   application terms will be transformed into real (!) applications
--   combinator symbols will be replaced by their actual function definition
link :: CombinatorDefinitions -> CExpr -> CExpr
link definitions (CApp fun arg) = link definitions fun ! link definitions arg
link definitions (CComb comb)   = case lookup comb definitions of
  Nothing -> resolveBulk comb
  Just e  -> e
link _definitions expr          = expr

linkLog :: CombinatorDefinitions -> CExpr -> CExpr
linkLog definitions (CApp fun arg) = link definitions fun ! link definitions arg
linkLog definitions (CComb comb)   = case lookup comb definitions of
  Nothing -> resolveBulk comb
  Just e  -> e
linkLog _definitions expr          = expr

-- | translate and link in one go
transLink :: CombinatorDefinitions -> CL -> CExpr
transLink definitions (fun :@ arg)  = transLink definitions fun ! transLink definitions arg
transLink _definitions (INT k)      = CInt k
transLink definitions (Com comb)    = case lookup comb definitions of
  Nothing -> resolveBulk comb
  Just e  -> e

transLinkLog :: CombinatorDefinitions -> CL -> CExpr
transLinkLog definitions (fun :@ arg)  = transLink definitions fun ! transLink definitions arg
transLinkLog _definitions (INT k)      = CInt k
transLinkLog definitions (Com comb)    = case lookup comb definitions of
  Nothing -> resolveBulkLog comb
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
  , R      --> CFun (\f -> CFun $ \g -> CFun $ \x -> g!x!f)            -- R F G X = G X F  
  , T      --> CFun (CFun . const)                                     -- T X Y = X
  , B'     --> comB' --CFun (\p -> CFun $ \q -> CFun $ \r -> CFun $ \s -> p!q!(r!s))      -- B' P Q R S = P Q (R S)
  , C'     --> comC' --CFun (\p -> CFun $ \q -> CFun $ \r -> CFun $ \s -> p!(q!s)!r)      -- C' P Q R S = P (Q S) R
  , S'     --> comS' --CFun (\p -> CFun $ \q -> CFun $ \r -> CFun $ \s -> p!(q!s)!(r!s))  -- S' P Q R S = P (Q S) (R S)
  , TRUE   --> CFun (CFun . const)                                     -- TRUE X Y = X (Scott encoded true)
  , FALSE  --> CFun (\_ -> CFun id)                                     -- FALSE X Y = Y (Scott encoded false)
  , Y      --> CFun (\(CFun f) -> fix f)
  , ADD    --> arith (+)
  , SUB    --> arith (-)
  , SUB1   --> CFun sub1
  , MUL    --> arith (*)
  , EQL    --> compArith (==)
  , GEQ    --> compArith (>=)
  , ZEROP  --> CFun isZero
  ]

resolveBulkLog :: Combinator -> CExpr
resolveBulkLog (BulkCom c n) = breakBulkLog (fromString c) n
  where
    breakBulkLog :: Combinator -> Int -> CExpr
    breakBulkLog c 1 = com c
    breakBulkLog B n = foldr ((!) . (bs!!)) comB (init $ bits n) where
      bs = [sbi, comB ! (comB ! comB) ! sbi]
    breakBulkLog c n = foldr ((!) . (bs!!)) (prime c) (init $ bits n) ! comI where
      bs = [sbi, comB ! (comB ! prime c) ! sbi]
      prime c = comB ! (comB ! com c) ! comB

    com :: Combinator -> CExpr
    com c = fromJust $ lookup c primitives
    sbi :: CExpr
    sbi = comS ! comB ! comI
    bits :: Integral t => t -> [t]
    bits n = r:if q == 0 then [] else bits q where (q, r) = divMod n 2

resolveBulk :: Combinator -> CExpr
resolveBulk (BulkCom "B" n) = iterate (comB' !) comB !! (n-1)
resolveBulk (BulkCom "C" n) = iterate (comC' !) comC !! (n-1)
resolveBulk (BulkCom "S" n) = iterate (comS' !) comS !! (n-1)
resolveBulk anyOther = error $ "not a known combinator: " ++ show anyOther

comI :: CExpr
comI = CFun id

comS :: CExpr
comS = CFun (\f -> CFun $ \g -> CFun $ \x -> f!x!(g!x))                    -- S F G X = F X (G X)

comS' :: CExpr
comS' = CFun (\p -> CFun $ \q -> CFun $ \r -> CFun $ \s -> p!(q!s)!(r!s))  -- S' P Q R S = P (Q S) (R S)

comB :: CExpr
comB = CFun (\f -> CFun $ \g -> CFun $ \x -> f!(g!x))                      -- B F G X = F (G X)
comB' :: CExpr
comB' = CFun (\p -> CFun $ \q -> CFun $ \r -> CFun $ \s -> p!q!(r!s))      -- B' P Q R S = P Q (R S)

comC :: CExpr
comC = CFun (\f -> CFun $ \g -> CFun $ \x -> f!x!g)                        -- C F G X = F X G
comC' :: CExpr
comC' = CFun (\p -> CFun $ \q -> CFun $ \r -> CFun $ \s -> p!(q!s)!r)      -- C' P Q R S = P (Q S) R

arith :: (Integer -> Integer -> Integer) -> CExpr
arith op = CFun $ \(CInt a) -> CFun $ \(CInt b) -> CInt (op a b)

-- Comparison operations that return TRUE/FALSE combinators
compArith :: (Integer -> Integer -> Bool) -> CExpr
compArith op = CFun $ \(CInt a) -> CFun $ \(CInt b) -> if op a b then CComb TRUE else CComb FALSE

eql :: (Eq a) => a -> a -> CExpr
eql n m = if n == m then CComb TRUE else CComb FALSE

geq :: (Ord a) => a -> a -> CExpr
geq n m = if n >= m then CComb TRUE else CComb FALSE

leq :: (Ord a) => a -> a -> CExpr
leq n m = if n <= m then CComb TRUE else CComb FALSE

gre :: (Ord a) => a -> a -> CExpr
gre n m = if n > m then CComb TRUE else CComb FALSE

le :: (Ord a) => a -> a -> CExpr
le n m = if n < m then CComb TRUE else CComb FALSE

sub1 :: CExpr -> CExpr
sub1 (CInt n) = CInt $ n -1
sub1 x        = error $ show x ++ " is not a number"

isZero :: CExpr -> CExpr
isZero (CInt n) = if n == 0 then CComb TRUE else CComb FALSE
isZero _        = CComb FALSE