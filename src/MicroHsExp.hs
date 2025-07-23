{-# LANGUAGE OverloadedStrings #-}
module MicroHsExp (
  toMhsPrg
) where

import CLTerm
import MicroHs.Exp
import MicroHs.Expr
import MicroHs.ExpPrint (toStringCMdl)
import MicroHs.Desugar (LDef)
import MicroHs.Ident

toMhsPrg :: CL -> String
toMhsPrg cl = 
  let (n, exps, prg) = toStringCMdl ([], toMhsExp cl)
   in prg

toMhsExp :: CL -> Exp
toMhsExp (Com c) = Lit (LPrim (combToMhscomb c))
toMhsExp (INT i) = Lit (LInt (fromIntegral i))
toMhsExp (t :@ u) = App (toMhsExp t) (toMhsExp u)

combToMhscomb :: Combinator -> String
combToMhscomb ADD = "+"
combToMhscomb SUB = "-"
combToMhscomb MUL = "*"
combToMhscomb DIV = "/"
combToMhscomb REM = "rem"
combToMhscomb EQL = "=="
combToMhscomb GEQ = ">="
combToMhscomb c = show c



