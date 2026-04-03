{-# LANGUAGE GADTs #-}

{-- | Semantic interpretation of categorical expressions (CatExpr) to functions.
    Interprets a CatExpr morphism as a function in the (->) category.

    > fnFst = interp (toCCC @CatExpr (\(x, y) -> x))
    > fnFst ("hello", "world")
    "hello"
--}

module CCC.Interpreter (interp) where

import           CCC.Cat     (BoolCat (andC, notC, orC),
                          BoolLike (false, true), Cartesian (dupC),
                          EqCat (eqlC), Monoidal (parC),
                          NumCat (addC, geqC, greC, leqC, lesC, mulC, subC))
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
interp Fst          = fst
interp Snd          = snd
interp Dup          = dupC
interp Add          = addC
interp Sub          = subC
interp Abs          = abs
interp Neg          = negate
interp Mul          = mulC
interp (Lift f)     = f
interp Eql          = eqlC
interp Leq          = leqC
interp Geq          = geqC
interp Les          = lesC
interp Gre          = greC
interp And          = andC
interp Or           = orC
interp Not          = notC
interp T            = const true
interp F            = const false
-- Conditional selects between two morphisms based on boolean test
interp IfThenElse   = \(test, (thenBranch, elseBranch)) ->
  if test then thenBranch else elseBranch
-- Value-level conditional: selects between two values based on boolean
interp IfVal        = \(test, (thenVal, elseVal)) ->
  if test then thenVal else elseVal
-- Fixpoint: step function is a CatExpr morphism, recursion stays categorical
interp (Fix step)   = \a ->
  let rec = Fix step
  in interp step (rec, a)