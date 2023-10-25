module TermReducer where

import CLTerm


-- data CL = Com Combinator | INT Integer | CL :@ CL

reduce :: CL -> IO CL
reduce (Com c) = pure $ Com c
reduce (INT i) = pure $ INT i
reduce (Com I :@ t) = pure t
reduce (Com K :@ t :@ _) = pure t
reduce (Com S :@ t :@ u :@ v) = pure $ (t :@ v) :@ (u :@ v)
reduce (Com B :@ f :@ g :@ x) = pure $ f :@ (g :@ x)      -- B F G X = F (G X)
reduce (Com C :@ t :@ u :@ v) = pure $ t :@ v :@ u
reduce (Com Y :@ t) = pure $ t :@ (Com Y :@ t)
reduce (Com P :@ t :@ u) = pure $ Com P :@ t :@ u
reduce (Com R :@ t :@ u) =  pure $ Com R :@ t :@ u
reduce (Com ADD :@ INT i :@ INT j) = pure $ INT (i + j)
reduce (Com ADD :@ i :@ j) = do ri <- red i; rj <- red j; reduce (Com ADD :@ ri :@ rj)
reduce (Com SUB :@ INT i :@ INT j) = pure $ INT (i - j)
reduce (Com SUB :@ i :@ j) = do ri <- red i; rj <- red j; reduce (Com SUB :@ ri :@ rj)
reduce (Com MUL :@ INT i :@ INT j) = pure $ INT (i * j)
reduce (Com MUL :@ i :@ j) = do ri <- red i; rj <- red j; reduce (Com MUL :@ ri :@ rj)
reduce (Com DIV :@ INT i :@ INT j) = pure $ INT (i `div` j)
reduce (Com DIV :@ i :@ j) = do ri <- red i; rj <- red j; reduce (Com DIV :@ ri :@ rj)
reduce (Com REM :@ INT i :@ INT j) = pure $ INT (i `rem` j)
reduce (Com REM :@ i :@ j) = do ri <- red i; rj <- red j; reduce (Com REM :@ ri :@ rj)
reduce (Com SUB1 :@ INT i) = pure $ INT (i - 1)
reduce (Com SUB1 :@ i) = do ri <- red i; reduce (Com SUB1 :@ ri)
reduce (Com EQL :@ INT i :@ INT j) = if i == j then pure $ INT 1 else pure $ INT 0
reduce (Com EQL :@ i :@ j) = do ri <- red i; rj <- red j; reduce (Com EQL :@ ri :@ rj)
reduce (Com GEQ :@ INT i :@ INT j) = if i >= j then pure $ INT 1 else pure $ INT 0
reduce (Com GEQ :@ i :@ j) = do ri <- red i; rj <- red j;  reduce (Com GEQ :@ ri :@ rj)
reduce (Com ZEROP :@ INT i) = if i == 0 then pure $ INT 1 else pure $ INT 0
reduce (Com ZEROP :@ i) = do ri <- red i; reduce (Com ZEROP :@ ri)
reduce (Com IF :@ (INT t) :@ u :@ v) = if t == 1 then red u else red v
reduce (Com IF :@ t :@ u :@ v) = do rt <- red t; if rt == INT 1 then red u else red v
reduce (Com B' :@ t :@ u :@ v) = pure $ t :@ (u :@ v)
reduce (Com C' :@ t :@ u :@ v) = pure $ t :@ v :@ u
reduce (Com S' :@ t :@ u :@ v) = pure $ (t :@ v) :@ (u :@ v)
reduce (Com T :@ t) = reduce t
reduce (t :@ u) = do rt <- red t; ru <- red u; reduce $ rt :@ ru

red :: CL -> IO CL
red x@(INT i) = do print x; pure x
red x@(Com c) = do print x; pure x
red x = do print x; red =<< reduce x