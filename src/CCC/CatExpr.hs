{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-- | GADT representing categorical expressions that instantiate type classes
    Closed, Cartesian, Category, and others. Serves as the compilation target for toCCC.

    Booleans are Scott-encoded as selector morphisms (CatExpr (a,a) a):
      TRUE  = Snd  (selects second, like A combinator)
      FALSE = Fst  (selects first, like K combinator)
    Conditionals are expressed as: Apply ∘ ⟨selector, ⟨thenVal, elseVal⟩⟩

    > toCCC @CatExpr (\(x, y) -> x)
    Comp Fst Id
--}

module CCC.CatExpr where

import           CCC.Cat     (Cartesian (..), Category (..), Closed (..),
                          EqCat (..), FixCat (..), Monoidal (..), NumCat (..), fanC)
import           Prelude hiding (id, (.))

data CatExpr a b where
  Comp :: CatExpr b c -> CatExpr a b -> CatExpr a c
  Id :: CatExpr a a
  IntConst :: Integer -> CatExpr a Integer
  FromInt :: (Num b) => CatExpr Integer b
  Fst :: CatExpr (a, b) a
  Snd :: CatExpr (a, b) b
  Dup :: CatExpr a (a, a)
  Par :: CatExpr a b -> CatExpr c d -> CatExpr (a, c) (b, d)
  Add :: (Num a) => CatExpr (a, a) a
  Sub :: (Num a) => CatExpr (a, a) a
  Mul :: (Num a) => CatExpr (a, a) a
  Abs :: (Num a) => CatExpr a a
  Neg :: (Num a) => CatExpr a a
  Apply :: CatExpr (CatExpr a b, a) b
  Curry :: CatExpr (a, b) c -> CatExpr a (CatExpr b c)
  Uncurry :: CatExpr a (CatExpr b c) -> CatExpr (a, b) c
  Lift :: (a -> b) -> CatExpr a b
  -- Comparison operators return Scott-encoded booleans (selector morphisms)
  -- A selector CatExpr (b,b) b picks one element from a pair:
  --   Snd = TRUE  (selects second, like A: λt e. e)
  --   Fst = FALSE (selects first, like K: λt e. t)
  Eql :: (Eq a) => CatExpr (a, a) (CatExpr (b, b) b)
  Leq :: (Ord a) => CatExpr (a, a) (CatExpr (b, b) b)
  Geq :: (Ord a) => CatExpr (a, a) (CatExpr (b, b) b)
  Les :: (Ord a) => CatExpr (a, a) (CatExpr (b, b) b)
  Gre :: (Ord a) => CatExpr (a, a) (CatExpr (b, b) b)
  -- Fixpoint combinator for recursive definitions
  Fix :: CatExpr (CatExpr a b, a) b -> CatExpr a b

instance Closed CatExpr where
  applyC = Apply
  curryC = Curry
  uncurryC = Uncurry

-- Hack required to auto-derive Show for CatExpr (allows function values in AST)
instance Show (a -> b) where
  showsPrec _ _ = showString "<function>"

deriving instance Show (CatExpr a b)

instance Category CatExpr where
  (.) = Comp
  id = Id

instance Monoidal CatExpr where
  parC = Par

instance Cartesian CatExpr where
  fstC = Fst
  sndC = Snd
  dupC = Dup

instance NumCat CatExpr where
  mulC = Mul
  negC = Neg
  addC = Add
  subC = Sub
  absC = Abs

  leqC = Leq
  geqC = Geq
  lesC = Les
  greC = Gre

instance (Num a) => Num (CatExpr z a) where
  f + g = Add . fanC f g
  f * g = Mul . fanC f g
  negate f = Neg . f
  f - g = Sub . fanC f g
  abs f = Abs . f
  signum = error "TODO sig"
  fromInteger i = FromInt . IntConst i

instance EqCat CatExpr where
  eqlC = Eql

instance FixCat CatExpr where
  fixC = Fix

-- Apply a morphism-valued expression to an argument
applyF :: CatExpr z (CatExpr a b) -> CatExpr z a -> CatExpr z b
applyF f x = Apply . fanC f x

instance Eq (CatExpr a b) where
  f == g = f Prelude.== g

