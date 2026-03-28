{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-- This module exposes a function interp that takes a (FreeCat a b) expression as input and returns a function
    of type (a -> b) which is the semantic interpretation of the CCC expression in the (->) category.

    > cccFst = simplify $ toCCC (\(x, y) -> x)
    > cccFst
    Fst
    > :t cccFst
    cccFst :: FreeCat (a, b) a
    > fnFst = interp cccFst
    > :t fnFst
    fnFst :: (a, b) -> a
    > fnFst ("hello", "world")
    "hello"
--}

module CCC.Interpreter (interp) where

import           CCC.Cat     (BoolCat (andC, ifTE, notC, orC),
                          BoolLike (false, true), Cartesian (dupC),
                          EqCat (eqlC), Monoidal (parC),
                          NumCat (addC, geqC, greC, leqC, lesC, mulC, subC),
                          applyC)
import           CCC.FreeCat (FreeCat (..))
import           CCC.Hask    ()

interp :: FreeCat a b -> (a -> b)
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
-- Fixpoint: step function is a FreeCat morphism, recursion stays categorical
interp (Fix step)   = \a ->
  let rec = Fix step  -- the recursive call is the Fix itself
  in interp step (rec, a)