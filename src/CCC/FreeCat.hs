{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-- This module exposes the GADT data type FreeCat which instantiates the type classes
    Closed, Cartesian and Category (among others).
    This makes this data type the ideal compilation target for toCCC.

    > toCCC @FreeCat (\(x, y) -> x)
    Comp Fst Id
--}    

module CCC.FreeCat where

import           CCC.Cat     (BoolCat (..), BoolLike (..), Cartesian (..), Cond (..),
                          Category (..), Closed (..), EqCat (..), EqLike (..),
                          FixCat (..), IfValCat (..), Monoidal (..), NumCat (..), fanC)
import           Prelude hiding (id, (.))
--import           Parser (Expr (..), Environment)

data FreeCat a b where
  Comp :: FreeCat b c -> FreeCat a b -> FreeCat a c
  Id :: FreeCat a a
  IntConst :: Integer -> FreeCat a Integer
  FromInt :: (Num b) => FreeCat Integer b
  Fst :: FreeCat (a, b) a
  Snd :: FreeCat (a, b) b
  Dup :: FreeCat a (a, a)
  Par :: FreeCat a b -> FreeCat c d -> FreeCat (a, c) (b, d)
  Add :: (Num a) => FreeCat (a, a) a
  Sub :: (Num a) => FreeCat (a, a) a
  Mul :: (Num a) => FreeCat (a, a) a
  Abs :: (Num a) => FreeCat a a
  Neg :: (Num a) => FreeCat a a
  Apply :: FreeCat (FreeCat a b, a) b
  Curry :: FreeCat (a, b) c -> FreeCat a (FreeCat b c)
  Uncurry :: FreeCat a (FreeCat b c) -> FreeCat (a, b) c
  Lift :: (a -> b) -> FreeCat a b
  Eql :: (EqLike a b, BoolLike b) => FreeCat (a, a) b
  Leq :: (Ord a, BoolLike b) => FreeCat (a, a) b
  Geq :: (Ord a, BoolLike b) => FreeCat (a, a) b
  Les :: (Ord a, BoolLike b) => FreeCat (a, a) b
  Gre :: (Ord a, BoolLike b) => FreeCat (a, a) b
  -- Boolean
  And :: (BoolLike a) => FreeCat (a, a) a
  Or :: (BoolLike a) => FreeCat (a, a) a
  Not :: (BoolLike a) => FreeCat a a
  T :: (BoolLike a) => FreeCat b a
  F :: (BoolLike a) => FreeCat b a
  -- Conditional branching: selects between two morphisms based on a boolean
  IfThenElse :: FreeCat (Bool, (FreeCat b c, FreeCat b c)) (FreeCat b c)
  -- Value-level conditional: selects between two values based on a boolean
  IfVal :: FreeCat (Bool, (a, a)) a
  -- Fixpoint combinator for recursive definitions
  -- Takes a step function (rec, input) -> output and produces the fixed point
  Fix :: FreeCat (FreeCat a b, a) b -> FreeCat a b

instance Closed FreeCat where
  applyC = Apply
  curryC = Curry
  uncurryC = Uncurry

-- this little hack is needed to allow auto deriving Show for FreeCat
instance Show (a -> b) where
  showsPrec _ _ = showString "<function>"

deriving instance Show (FreeCat a b)

instance Category FreeCat where
  (.) = Comp
  id = Id

instance Monoidal FreeCat where
  parC = Par

instance Cartesian FreeCat where
  fstC = Fst
  sndC = Snd
  dupC = Dup

instance NumCat FreeCat where
  mulC = Mul
  negC = Neg
  addC = Add
  subC = Sub
  absC = Abs

  leqC = Leq
  geqC = Geq
  lesC = Les
  greC = Gre

instance (Num a) => Num (FreeCat z a) where
  f + g = Add . fanC f g
  f * g = Mul . fanC f g
  negate f = Neg . f
  f - g = Sub . fanC f g
  abs f = Abs . f
  signum = error "TODO sig"
  fromInteger i = FromInt . IntConst i --error "TODO fromInteger"

instance BoolCat FreeCat where
  andC = And
  orC = Or
  notC = Not

  ifTE = IfThenElse

instance (BoolLike b) => BoolLike (FreeCat a b) where
  f && g = And . fanC f g
  f || g = Or . fanC f g
  not f = Not . f
  true = T
  false = F

--ite :: FreeCat a b -> (FreeCat c d, FreeCat c d) -> FreeCat c d
--ite test (f,g) = undefined --_IfThenElse . test . fanC f g

instance EqCat FreeCat where
  eqlC = Eql

instance IfValCat FreeCat where
  ifValC = IfVal

instance FixCat FreeCat where
  fixC = Fix

-- Cond instance for FreeCat: ite combines condition and branches into IfVal
-- condition :: FreeCat z Bool, branches :: FreeCat z a, result :: FreeCat z a
instance Cond (FreeCat z Bool) (FreeCat z a) where
  ite cond thenBranch elseBranch = IfVal . fanC cond (fanC thenBranch elseBranch)

-- Apply a morphism-valued expression to an argument
-- applyF f x = Apply . fanC f x
-- This is needed for higher-order functions where 'f' is a path to a morphism
applyF :: FreeCat z (FreeCat a b) -> FreeCat z a -> FreeCat z b
applyF f x = Apply . fanC f x

-- Equality comparison with fixed result type (avoids ambiguity)
eqF :: (EqLike a Bool) => FreeCat z a -> FreeCat z a -> FreeCat z Bool
eqF f g = Eql . fanC f g

-- Conditional with fixed condition type (avoids ambiguity)
iteF :: FreeCat z Bool -> FreeCat z a -> FreeCat z a -> FreeCat z a
iteF cond t e = IfVal . fanC cond (fanC t e)

-- General instance for comparing FreeCat morphisms with same domain
-- This enables (n == 0) where n :: FreeCat z Integer and 0 :: FreeCat z Integer
instance {-# OVERLAPPABLE #-} (BoolLike b, EqLike a b) => EqLike (FreeCat z a) (FreeCat z b) where
  f == g = Eql . fanC f g

-- Instance for comparing plain values to produce FreeCat (used by toCCC for is0)
instance {-# OVERLAPPABLE #-} (BoolLike b, EqLike a b) => EqLike a (FreeCat a b) where
  x == y = Eql . fanC (Lift (const x)) (Lift (const y))

instance Eq (FreeCat a b) where
  f == g = f Prelude.== g

