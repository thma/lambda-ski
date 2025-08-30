{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveDataTypeable #-}
module CLTerm
  (
    CL(..),
    Combinator(..),
    fromString,
    toCL,
    LeftAncestors,
    leftAncestors,
    trueCL,
    falseCL
  )
  where

import Parser (Expr(..))
import Data.Data(Data)

data CL = Com Combinator | INT Integer | CL :@ CL deriving (Eq, Data)

instance Show CL where
  showsPrec :: Int -> CL -> ShowS
  showsPrec p = \case
    Com c -> (if p > 0 then (' ':) else id) . (toString c ++)
    INT i -> ((' ': show i )++)
    t :@ u -> showParen (p > 0) $ shows t . showsPrec 1 u  
    where 
      toString :: Combinator -> String
      toString (BulkCom c n) = c ++ show n
      toString c = show c

type LeftAncestors = [CL]

leftAncestors :: CL -> LeftAncestors
leftAncestors clTerm = leftAncestors' clTerm []
  where
    leftAncestors' :: CL -> LeftAncestors -> LeftAncestors
    leftAncestors' (t :@ u) stack = leftAncestors' t (u : stack)
    leftAncestors' t stack = t : stack


data Combinator = I | K | S | B | C | Y | P | R | ADD | SUB | MUL | DIV | REM | SUB1 | EQL | GEQ | LEQ | ZEROP | 
                 A | B' | C' | S' | T | 
                 BulkCom String Int
  deriving (Eq, Show, Data)

fromString :: String -> Combinator
fromString "i"    = I
fromString "k"    = K
fromString "s"    = S
fromString "b"    = B
fromString "c"    = C
fromString "a"    = A
fromString "s'"   = S'
fromString "b'"   = B'
fromString "c'"   = C'
fromString "y"    = Y
fromString "p"    = P
fromString "r"    = R
fromString "+"    = ADD
fromString "sub"  = SUB
fromString "-"    = SUB
fromString "div"  = DIV
fromString "rem"  = REM
fromString "*"    = MUL
fromString "sub1" = SUB1
fromString "eql"  = EQL
fromString "geq"  = GEQ
fromString "leq"  = LEQ
fromString "is0"  = ZEROP
fromString "A"    = A
fromString "B"    = B
fromString "C"    = C
fromString "S"    = S
fromString "T"    = T
fromString _c     = error $ "unknown combinator " ++ _c 

toCL :: Expr -> CL
toCL (Var s) = Com (fromString s)
toCL (Lam x e) = error "toCL: lambda expression not in normal form"
toCL (m `App` n) = toCL m :@ toCL n
toCL (Int i) = INT i

-- | Scott-encoded TRUE and FALSE using basic SKI combinators
-- TRUE  = λt e. e = A (selects second argument)
-- FALSE = λt e. t = K (selects first argument)
trueCL :: CL
trueCL = Com A

falseCL :: CL 
falseCL = Com K