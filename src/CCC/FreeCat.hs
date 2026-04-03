{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-- | GADT representing categorical expressions that instantiate type classes
    Closed, Cartesian, Category, and others. Serves as the compilation target for toCCC.

    > toCCC @CatExpr (\(x, y) -> x)
    Comp Fst Id
--}

module CCC.CatExpr where

import           CCC.Cat     (BoolCat (..), BoolLike (..), Cartesian (..), Cond (..),
                          Category (..), Closed (..), EqCat (..), EqLike (..),
                          FixCat (..), IfValCat (..), Monoidal (..), NumCat (..), fanC)
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
  Eql :: (EqLike a b, BoolLike b) => CatExpr (a, a) b
  Leq :: (Ord a, BoolLike b) => CatExpr (a, a) b
  Geq :: (Ord a, BoolLike b) => CatExpr (a, a) b
  Les :: (Ord a, BoolLike b) => CatExpr (a, a) b
  Gre :: (Ord a, BoolLike b) => CatExpr (a, a) b
  -- Boolean combinators
  And :: (BoolLike a) => CatExpr (a, a) a
  Or :: (BoolLike a) => CatExpr (a, a) a
  Not :: (BoolLike a) => CatExpr a a
  T :: (BoolLike a) => CatExpr b a
  F :: (BoolLike a) => CatExpr b a
  -- Conditional branching: selects between morphisms based on a boolean
  IfThenElse :: CatExpr (Bool, (CatExpr b c, CatExpr b c)) (CatExpr b c)
  -- Value-level conditional: selects between values based on a boolean
  IfVal :: CatExpr (Bool, (a, a)) a
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

instance BoolCat CatExpr where
  andC = And
  orC = Or
  notC = Not

  ifTE = IfThenElse

instance (BoolLike b) => BoolLike (CatExpr a b) where
  f && g = And . fanC f g
  f || g = Or . fanC f g
  not f = Not . f
  true = T
  false = F

instance EqCat CatExpr where
  eqlC = Eql

instance IfValCat CatExpr where
  ifValC = IfVal

instance FixCat CatExpr where
  fixC = Fix

-- Cond instance for CatExpr: combines condition and branches
instance Cond (CatExpr z Bool) (CatExpr z a) where
  ite cond thenBranch elseBranch = IfVal . fanC cond (fanC thenBranch elseBranch)

-- Apply a morphism-valued expression to an argument
applyF :: CatExpr z (CatExpr a b) -> CatExpr z a -> CatExpr z b
applyF f x = Apply . fanC f x

-- Equality comparison with fixed result type (avoids ambiguity)
eqF :: (EqLike a Bool) => CatExpr z a -> CatExpr z a -> CatExpr z Bool
eqF f g = Eql . fanC f g

-- Conditional with fixed condition type (avoids ambiguity)
iteF :: CatExpr z Bool -> CatExpr z a -> CatExpr z a -> CatExpr z a
iteF cond t e = IfVal . fanC cond (fanC t e)

-- General instance for comparing categorical morphisms
instance {-# OVERLAPPABLE #-} (BoolLike b, EqLike a b) => EqLike (CatExpr z a) (CatExpr z b) where
  f == g = Eql . fanC f g

-- Instance for comparing plain values to produce CatExpr (used by toCCC)
instance {-# OVERLAPPABLE #-} (BoolLike b, EqLike a b) => EqLike a (CatExpr a b) where
  x == y = Eql . fanC (Lift (const x)) (Lift (const y))

instance Eq (CatExpr a b) where
  f == g = f Prelude.== g

