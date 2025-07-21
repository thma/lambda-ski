{-# LANGUAGE DeriveDataTypeable #-}
module TermReducer where

import Data.Generics.Uniplate.Data
import CLTerm


reduce :: CL -> CL
reduce (Com c) = Com c
reduce (INT i) = INT i
reduce (Com I :@ t) = t
reduce (Com K :@ t :@ _) = t
reduce (Com S :@ t :@ u :@ v) = (t :@ v) :@ (u :@ v)
reduce (Com B :@ f :@ g :@ x) = f :@ (g :@ x)      -- B F G X = F (G X)
reduce (Com C :@ t :@ u :@ v) = t :@ v :@ u
reduce (Com Y :@ t) = t :@ (Com Y :@ t)
reduce (Com P :@ t :@ u) = Com P :@ t :@ u
reduce (Com R :@ t :@ u) = Com R :@ t :@ u
reduce (Com ADD :@ INT i :@ INT j) = INT (i + j)
reduce (Com ADD :@ i :@ j) = reduce (Com ADD :@ red i :@ red j)
reduce (Com SUB :@ INT i :@ INT j) = INT (i - j)
reduce (Com SUB :@ i :@ j) = reduce (Com SUB :@ red i :@ red j)
reduce (Com MUL :@ INT i :@ INT j) = INT (i * j)
reduce (Com MUL :@ i :@ j) = reduce (Com MUL :@ red i :@ red j)
reduce (Com DIV :@ INT i :@ INT j) = INT (i `div` j)
reduce (Com DIV :@ i :@ j) = reduce (Com DIV :@ red i :@ red j)
reduce (Com REM :@ INT i :@ INT j) = INT (i `rem` j)
reduce (Com REM :@ i :@ j) = reduce (Com REM :@ red i :@ red j)
reduce (Com SUB1 :@ INT i) = INT (i - 1)
reduce (Com SUB1 :@ i) = reduce (Com SUB1 :@ red i)
reduce (Com EQL :@ INT i :@ INT j) = if i == j then trueCL else falseCL
reduce (Com EQL :@ i :@ j) = reduce (Com EQL :@ red i :@ red j)
reduce (Com GEQ :@ INT i :@ INT j) = if i >= j then trueCL else falseCL
reduce (Com GEQ :@ i :@ j) = reduce (Com GEQ :@ red i :@ red j)
reduce (Com ZEROP :@ INT i) = if i == 0 then trueCL else falseCL
reduce (Com ZEROP :@ i) = reduce (Com ZEROP :@ red i)
reduce (Com B' :@ t :@ u :@ v) = t :@ (u :@ v)
reduce (Com C' :@ t :@ u :@ v) = t :@ v :@ u
reduce (Com S' :@ t :@ u :@ v) = (t :@ v) :@ (u :@ v)
reduce (Com T :@ t) = t
reduce x = error $ "not a known combinator: " ++ show x


red :: CL -> CL
red = transform reduce

-- red :: CL -> IO CL
-- red x@(INT i) = do print x; pure x
-- red x@(Com c) = do print x; pure x
-- red x = do print x; red =<< reduce x