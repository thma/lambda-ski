{-# LANGUAGE OverloadedStrings #-}
module MicroHsExp where

import CLTerm
import MicroHs.Exp
import MicroHs.Expr
import MicroHs.ExpPrint (toStringCMdl)
import MicroHs.Desugar (LDef)
import MicroHs.Ident

{--

data Exp
  = Var Ident
  | App Exp Exp
  | Lam Ident Exp
  | Lit Lit
  deriving (Eq)

type LDef = (Ident, Exp)

data SLoc = SLoc FilePath Line Col
type Line = Int
type Col  = Int

data Ident = Ident SLoc Text


-- The argument is all definitions and the main expression.
-- The result is the number of definitions, all foreign export identifiers, and the program as a string.
toStringCMdl :: ([LDef], Exp) -> (Int, [Ident], String)


--}

example :: Exp
example = Lit (LInt 42)
  --Var sampleIdent
  --`App` Lam sampleIdent (Var sampleIdent)
  --`App` Lit (LInt 42)

sampleIdent :: Ident
sampleIdent = mkIdent "sample"

samplePrg :: ([LDef], Exp)
samplePrg = ([(sampleIdent, example)], example)

addExp :: Exp
addExp = App (Lit (LPrim "IO.print")) (App (App (Lit (LPrim "+")) (Lit (LInt 42))) (Lit (LInt 23)))

addPrg :: ([LDef], Exp)
addPrg = ([], addExp)


toMhsExp :: CL -> Exp
toMhsExp (Com c) = Lit (LPrim (show c))
toMhsExp (INT i) = Lit (LInt (fromIntegral i))
toMhsExp (t :@ u) = App (toMhsExp t) (toMhsExp u)

toMhsPrg :: CL -> String
toMhsPrg cl = let (n, exps, prg) = toStringCMdl ([], toMhsExp cl)
              in prg

toIon :: CL -> String
toIon (Com c) = show c
toIon (INT i) = "(" ++ show i ++ ")"
toIon (t :@ u) = "`" ++ toIon t  ++ toIon u

test :: CL
test = Com B :@ Com S :@ (Com B :@ Com B)

main :: IO ()
main = do

  let (n, exps, prg) = toStringCMdl addPrg
  putStrLn $ "Number of definitions: " ++ show n
  putStrLn $ "Foreign exports: " ++ show exps
  putStrLn $ "Program: " ++ prg

  -- let (n, exps, prg) = toStringCMdl samplePrg
  -- putStrLn $ "Number of definitions: " ++ show n
  -- putStrLn $ "Foreign exports: " ++ show exps
  -- putStrLn $ "Program: " ++ prg

  -- let mhsExp = toMhsExp test
  -- putStrLn $ "MicroHs expression: " ++ show mhsExp

  -- putStrLn $ "Test expression: " ++ (show $ toStringCMdl ([], mhsExp))