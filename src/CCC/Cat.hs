{-- This module contains definition of categories that are required for
    modelling Closed Cartesian Categories.
  It re-exports Category from Control.Category and defines Monoidal, Cartesian and Closed.

  Booleans are Scott-encoded as selector morphisms:
    TRUE  = sndC  (selects second from a pair, like A combinator: λt e. e)
    FALSE = fstC  (selects first from a pair, like K combinator: λt e. t)
  Comparison operators return selector morphisms k (b,b) b instead of Bool values.
  Conditionals are just application of the selector to a pair of alternatives.
    --}

module CCC.Cat
  ( Category (..)
  , Monoidal (..)
  , Cartesian (..)
  , Closed (..)
  , fanC
  , idC
  , NumCat (..)
  , EqCat (..)
  , FixCat (..)
  ) where

import           Control.Category (Category (..))
import           Prelude hiding (id, (.))

class Category k => Monoidal k where
  parC :: k a c -> k b d -> k (a, b) (c, d)

class Monoidal k => Cartesian k where
  fstC :: k (a, b) a
  sndC :: k (a, b) b
  dupC :: k a (a, a)

class Cartesian k => Closed k where
  applyC :: k (k a b, a) b
  curryC :: k (a, b) c -> k a (k b c)
  uncurryC :: k a (k b c) -> k (a, b) c

fanC :: (Cartesian cat) => cat b c -> cat b d -> cat b (c, d)
fanC f g = parC f g . dupC

idC :: (Category k) => k a a
idC = id

class Cartesian k => NumCat k where
  mulC :: Num a => k (a, a) a
  negC :: Num a => k a a
  addC :: Num a => k (a, a) a
  subC :: Num a => k (a, a) a
  absC :: Num a => k a a
  -- Comparison operators return Scott-encoded booleans (selector morphisms)
  leqC :: Ord a => k (a, a) (k (b, b) b)
  geqC :: Ord a => k (a, a) (k (b, b) b)
  lesC :: Ord a => k (a, a) (k (b, b) b)
  greC :: Ord a => k (a, a) (k (b, b) b)

-- Equality comparison returns a Scott-encoded boolean (selector morphism)
class Cartesian k => EqCat k where
  eqlC :: Eq a => k (a, a) (k (b, b) b)

-- Fixpoint combinator for recursive definitions
-- Takes a step function (rec, input) -> output and produces the fixed point
class Closed k => FixCat k where
  fixC :: k (k a b, a) b -> k a b
