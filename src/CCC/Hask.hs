{-# OPTIONS_GHC -fno-warn-orphans #-}

{-- | Type class instances for (->) enabling interpretation of CatExpr as functions.
    Allows categorical expressions to be evaluated as standard Haskell functions.
--}

module CCC.Hask where

import           CCC.Cat
import qualified CCC.Cat as Cat

instance Monoidal (->) where
  parC f g (x, y) = (f x, g y) -- this could also be implemented as `bimap f g` (imported from Data.Bifunctor)

instance Cartesian (->) where
  fstC (x, _y) = x
  sndC (_x, y) = y
  dupC x = (x, x)

instance Closed (->) where
  applyC (f, x) = f x
  curryC = curry
  uncurryC = uncurry

instance NumCat (->) where
  mulC = uncurry (*)
  negC = negate
  addC = uncurry (+)
  subC = uncurry (-)
  absC = abs
  -- Need explicit type annotations since BoolLike is polymorphic
  leqC (x, y) = if x <= y then true else false
  geqC (x, y) = if x >= y then true else false
  lesC (x, y) = if x < y then true else false
  greC (x, y) = if x > y then true else false

instance BoolCat (->) where
  andC = uncurry (Cat.&&)
  orC = uncurry (Cat.||)
  notC = Cat.not
  ifTE (test, (f, g)) x = if test then f x else g x

instance EqCat (->) where
  eqlC = uncurry (Cat.==)

instance IfValCat (->) where
  ifValC (test, (t, e)) = if test then t else e

instance FixCat (->) where
  -- The step function takes (rec, input) and produces output
  -- We tie the knot by making rec = fixC step
  fixC step = let f a = step (f, a) in f