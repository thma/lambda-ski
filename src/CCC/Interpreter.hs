{-# LANGUAGE GADTs #-}

{-- | Semantic interpretation of categorical expressions (CatExpr) to functions.
    Interprets a CatExpr morphism as a function in the (->) category.

    Booleans are Scott-encoded as selector morphisms:
      TRUE  = Snd (selects second, like A combinator: λt e. e)
      FALSE = Fst (selects first, like K combinator: λt e. t)
    Comparison operators return Snd or Fst as CatExpr values.
    Conditionals are expressed as: Apply ∘ ⟨selector, ⟨thenVal, elseVal⟩⟩

    > fnFst = interp (toCCC @CatExpr (\(x, y) -> x))
    > fnFst ("hello", "world")
    "hello"
--}

module CCC.Interpreter (interp) where

import           CCC.Cat     (Cartesian (fstC, sndC, dupC), Monoidal (parC),
                          NumCat (addC, mulC, subC))
import           CCC.CatExpr (CatExpr (..))
import           CCC.Hask    ()

interp :: CatExpr a b -> (a -> b)
interp (Comp f g)   = interp f . interp g
interp (Par f g)    = parC (interp f) (interp g)
interp (Curry f)    = Lift . curry (interp f)
interp (Uncurry f)  = \(a, b) -> interp (interp f a) b
interp Apply        = uncurry interp
interp Id           = id
interp (IntConst i) = const i
interp FromInt      = fromInteger
interp Fst          = fstC
interp Snd          = sndC
interp Dup          = dupC
interp Add          = addC
interp Sub          = subC
interp Abs          = abs
interp Neg          = negate
interp Mul          = mulC
interp (Lift f)     = f
-- Comparison operators return Scott-encoded booleans:
-- TRUE = Snd (selects second), FALSE = Fst (selects first)
interp Eql          = \(x, y) -> if x == y then Snd else Fst
interp Leq          = \(x, y) -> if x <= y then Snd else Fst
interp Geq          = \(x, y) -> if x >= y then Snd else Fst
interp Les          = \(x, y) -> if x < y then Snd else Fst
interp Gre          = \(x, y) -> if x > y then Snd else Fst
-- Fixpoint: step function is a CatExpr morphism, recursion stays categorical
interp (Fix step)   = \a ->
  let rec = Fix step
  in interp step (rec, a)