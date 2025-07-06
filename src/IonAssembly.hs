module IonAssembly where

import CLTerm

{--
digit = '0' | ... | '9'
num = digit [num]
con = '#' CHAR | '(' num ')'
idx = '@' CHAR | '[' num ']'
comb = 'S' | 'K' | 'I' | ...
term = comb | '`' term term | con | idx
prog = term ';' [prog]
--}


toIon :: CL -> String
toIon (Com c) = show c
toIon (INT i) = "(" ++ show i ++ ")"
toIon (t :@ u) = "`" ++ toIon t  ++ toIon u

test :: CL
test = Com B :@ Com S :@ (Com B :@ Com B)