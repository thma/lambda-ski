module TermReducer where

import Data.Generics.Uniplate.Data
import CLTerm
import Debug.Trace

-- reduce :: Exp -> Exp
-- reduce (((S :@ x) :@ y) :@ z) = (x :@ z) :@ (y :@ z)
-- reduce ((((S' :@ x) :@ y) :@ z) :@ w) = (x :@ (y :@ w)) :@ (z :@ w)
-- reduce ((K :@ x) :@ _y) = x
-- reduce ((A :@ _x) :@ y) = y
-- reduce ((U :@ x) :@ y) = y :@ x
-- reduce (I :@ x) = x
-- reduce (((B :@ x) :@ y) :@ z) = x :@ (y :@ z)
-- reduce ((((B' :@ x) :@ y) :@ z) :@ w) = (x :@ y) :@ (z :@ w)
-- reduce (((Z :@ x) :@ y) :@ _z) = x :@ y
-- reduce (((C :@ x) :@ y) :@ z) = (x :@ z) :@ y
-- reduce ((((C' :@ x) :@ y) :@ z) :@ w) = (x :@ (y :@ w)) :@ z
-- reduce (((P :@ x) :@ y) :@ z) = (z :@ x) :@ y
-- reduce (((R :@ x) :@ y) :@ z) = (y :@ z) :@ x
-- reduce ((((O :@ x) :@ y) :@ z) :@ w) = (w :@ x) :@ y
-- reduce (((K2 :@ x) :@ _y) :@ _z) = x
-- reduce ((((K3 :@ x) :@ _y) :@ _z) :@ _w) = x
-- reduce (((((K4 :@ x) :@ _y) :@ _z) :@ _w) :@ _v) = x
-- reduce ((((C'B :@ x) :@ y) :@ z) :@ w) = (x :@ z) :@ (y :@ w)
-- reduce (Label _ e) = e
-- reduce (Tick _ :@ e) = e
-- reduce e = e

-- | Single step reduction - reduces only the outermost redex
reduceStep :: CL -> CL
reduceStep (Com c) = Com c
reduceStep (INT i) = INT i
reduceStep (Com I :@ t) = t
reduceStep (Com K :@ t :@ u) = t
reduceStep (Com S :@ x :@ y :@ z) = (x :@ z) :@ (y :@ z)
reduceStep (Com B :@ f :@ g :@ x) = f :@ (g :@ x)      -- B F G X = F (G X)
reduceStep (Com C :@ x :@ y :@ z) = x :@ z :@ y
reduceStep yt@(Com Y :@ t) = t :@ yt
reduceStep (Com P :@ t :@ u) = Com P :@ t :@ u
reduceStep (Com R :@ t :@ u) = Com R :@ t :@ u
reduceStep (Com ADD :@ INT i :@ INT j) = INT (i + j)
reduceStep (Com SUB :@ INT i :@ INT j) = INT (i - j)
reduceStep (Com MUL :@ INT i :@ INT j) = INT (i * j)
reduceStep (Com DIV :@ INT i :@ INT j) = INT (i `div` j)
reduceStep (Com REM :@ INT i :@ INT j) = INT (i `rem` j)
reduceStep (Com SUB1 :@ INT i) = INT (i - 1)
reduceStep (Com EQL :@ INT i :@ INT j) = if i == j then trueCL else falseCL
reduceStep (Com GEQ :@ INT i :@ INT j) = if i >= j then trueCL else falseCL
reduceStep (Com ZEROP :@ INT i) = if i == 0 then trueCL else falseCL
reduceStep (Com ZEROP :@ i) = Com ZEROP :@ (reduce i)  -- Keep ZEROP combinator for non-integer terms
reduceStep (Com B' :@ t :@ u :@ v) = t :@ (u :@ v)
reduceStep (Com C' :@ t :@ u :@ v) = t :@ v :@ u
reduceStep (Com S' :@ t :@ u :@ v) = (t :@ v) :@ (u :@ v)
reduceStep (Com T :@ t) = t
reduceStep (Com A :@ x :@ y) = y  -- A combinator: λx y. y (like FALSE)
-- For partial applications, try to reduce arguments
reduceStep (f :@ x) = 
  let f' = reduce f
  in if f' == f 
     then f :@ reduceStep x  -- Only reduce argument if function can't be reduced
     else f' :@ x           -- Apply reduction to function first
reduceStep x = x

-- | Reduce with step limit to avoid infinite loops
reduceWithLimit :: Int -> CL -> CL
reduceWithLimit 0 x = x
reduceWithLimit n x = 
  let x' = reduceStep (trace (show x) x)
  in if x' == x 
     then x  -- Normal form reached
     else reduceWithLimit (n-1) x'

-- | Original reduce function - now with step limit
reduce :: CL -> CL
reduce = reduceWithLimit 50  -- Allow up to 50 reduction steps

-- | Deep reduction using transform (original approach)
redDeep :: CL -> CL
redDeep = transform reduceStep

-- | Main reduction function - tries deep reduction first, then limited reduction
red :: CL -> CL
red x = 
  let deepResult = redDeep x
  in if deepResult == x
     then reduce x  -- Try step-by-step if deep reduction doesn't help
     else reduce deepResult  -- Apply step-by-step to the deep result