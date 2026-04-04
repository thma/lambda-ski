{-# OPTIONS_GHC -fno-warn-orphans #-}

{-- | Instance-only module for interpreting categorical terms in the (->) category. 
    This module provides the necessary type class instances to treat Haskell functions 
    as morphisms in the (->) category, allowing us to interpret CatExpr morphisms 
    as actual Haskell functions without needing to define the logic of those functions ourselves.

    Booleans are Scott-encoded: comparison operators return selector functions
    that pick the first or second element from a pair.
      TRUE  = sndC (selects second, like A combinator)
      FALSE = fstC (selects first, like K combinator)
--}

module CCC.Hask () where

import           CCC.Cat

instance Monoidal (->) where
  parC f g (x, y) = (f x, g y)

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
  -- Comparisons return Scott-encoded booleans (selector functions)
  leqC (x, y) = if x <= y then sndC else fstC
  geqC (x, y) = if x >= y then sndC else fstC
  lesC (x, y) = if x < y then sndC else fstC
  greC (x, y) = if x > y then sndC else fstC

instance EqCat (->) where
  eqlC (x, y) = if x == y then sndC else fstC

instance FixCat (->) where
  fixC step = let f a = step (f, a) in f