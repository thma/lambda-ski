{-# OPTIONS_GHC -fno-warn-orphans #-}

{-- | Instance-only module for interpreting categorical terms in the (->) category. 
    This module provides the necessary type class instances to treat Haskell functions 
    as morphisms in the (->) category, allowing us to interpret CatExpr morphisms 
    as actual Haskell functions without needing to define the logic of those functions ourselves.
--}

module CCC.Hask () where

import           CCC.Cat
import qualified CCC.Cat as Cat

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
  leqC = ordPred (<=)
  geqC = ordPred (>=)
  lesC = ordPred (<)
  greC = ordPred (>)

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
  fixC step = let f a = step (f, a) in f

-- BoolLike result type prevents using plain uncurry comparison directly.
ordPred :: (Ord a, BoolLike b) => (a -> a -> Bool) -> (a, a) -> b
ordPred op (x, y) = if op x y then true else false