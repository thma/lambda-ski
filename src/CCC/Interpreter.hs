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
   
import           CCC.CatExpr (CatExpr (..), scottBool)
--import           CCC.Cat  
--import           CCC.Hask    ()

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
-- scottBool reifies Haskell Bool into CatExpr selectors (TRUE=Snd, FALSE=Fst)
interp Eql          = scottBool . uncurry (==)
interp Leq          = scottBool . uncurry (<=)
interp Geq          = scottBool . uncurry (>=)
interp Les          = scottBool . uncurry (<)
interp Gre          = scottBool . uncurry (>)
-- Fixpoint: step function is a CatExpr morphism, recursion stays categorical
interp (Fix step)   = \a ->
  let rec = Fix step
  in interp step (rec, a)

parC :: (a -> b) -> (c -> d) -> ((a, c) -> (b, d))
parC f g (a, c) = (f a, g c)

fstC :: (a, b) -> a
fstC = fst

sndC :: (a, b) -> b
sndC = snd

dupC :: a -> (a, a)
dupC x = (x, x)

addC :: (Num a) => (a, a) -> a
addC = uncurry (+)

subC :: (Num a) => (a, a) -> a
subC = uncurry (-)

mulC :: (Num a) => (a, a) -> a
mulC = uncurry (*)