module TermReducer where

import Data.Generics.Uniplate.Data (descend)
import Control.Monad (ap)
import CLTerm


-- | Single step reduction - reduces only the outermost redex
reduceStep :: CL -> CL
reduceStep (Com c) = Com c
reduceStep (INT i) = INT i
reduceStep (Com I :@ t) = t
reduceStep ((Com K :@ t) :@ u) = t
reduceStep (((Com S :@ x) :@ y) :@ z) = (x :@ z) :@ (y :@ z)
reduceStep (((Com B :@ f) :@ g) :@ x) = f :@ (g :@ x)       -- B F G X = F (G X)
reduceStep (((Com C :@ x) :@ y) :@ z) = x :@ z :@ y
reduceStep ((Com Y :@ f) :@ x) = (f :@ (Com Y :@ f)) :@ x  -- Y F X = (F (Y F)) X
reduceStep (((Com R :@ f) :@ g) :@ x) = (g :@ x) :@ f
reduceStep ((Com ADD :@ INT i) :@ INT j) = INT (i + j)
reduceStep ((Com SUB :@ INT i) :@ INT j) = INT (i - j)
reduceStep ((Com MUL :@ INT i) :@ INT j) = INT (i * j)
reduceStep ((Com DIV :@ INT i) :@ INT j) = INT (i `div` j)
reduceStep ((Com REM :@ INT i) :@ INT j) = INT (i `rem` j)
reduceStep (Com SUB1 :@ INT i) = INT (i - 1)
reduceStep ((Com EQL :@ INT i) :@ INT j) = if i == j then trueCL else falseCL
reduceStep ((Com GEQ :@ INT i) :@ INT j) = if i >= j then trueCL else falseCL
reduceStep (Com ZEROP :@ INT i) = if i == 0 then trueCL else falseCL
reduceStep (Com ZEROP :@ i) = Com ZEROP :@ reduceStep i  -- Reduce argument without calling full reduce
reduceStep ((((Com B' :@ p) :@ q) :@ r) :@ s) = (p :@ q) :@ (r :@ s) -- B' P Q R S = P Q (R S)
reduceStep ((((Com C' :@ p) :@ q) :@ r) :@ s) = (p :@ (q :@ s)) :@ r -- C' P Q R S = P (Q S) R
reduceStep ((((Com S' :@ p) :@ q) :@ r) :@ s) = (p :@ (q :@ s)) :@ (r :@ s) -- S' P Q R S = P (Q S) (R S)
reduceStep (Com T :@ t) = t
reduceStep ((Com A :@ x) :@ y) = y  -- A combinator: λx y. y (like TRUE, selects second)
-- For partial applications, don't reduce recursively in reduceStep
reduceStep (f :@ x) = f :@ x  -- No reduction for general applications



-- | reduction using uniplate with custom traversal
-- Using 'until' from Prelude to iterate until fixed point
reduce :: CL -> CL
reduce = until (ap (==) reduceOnce) reduceOnce

-- | One-step reduction with leftmost-outermost strategy
-- Uses descend to try reduction at each level
reduceOnce :: CL -> CL  
reduceOnce term =
  -- First try to reduce at the root
  let rootReduced = reduceStep term
  in if rootReduced /= term
     then rootReduced
     else 
       -- If root doesn't reduce, use descend to try one level down
       -- descend applies function to immediate children
       let descended = descend reduceOnce term
       in if descended /= term
          then descended
          else term
