{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module CLTerm
  (
    CL(..),
    Combinator(..),
    fromString,
    toCL,
    LeftAncestors,
    leftAncestors
  )
  where

import Parser (Expr(..))

data CL = Com Combinator | INT Integer | CL :@ CL
instance Show CL where
  showsPrec :: Int -> CL -> ShowS
  showsPrec p = \case
    Com s -> (if p > 0 then (' ':) else id) . (show s ++)
    INT i -> ((' ': show i )++)
    t :@ u -> showParen (p > 0) $ shows t . showsPrec 1 u   

type LeftAncestors = [CL]

leftAncestors :: CL -> LeftAncestors
leftAncestors clTerm = leftAncestors' clTerm []
  where
    leftAncestors' :: CL -> LeftAncestors -> LeftAncestors
    leftAncestors' (t :@ u) stack = leftAncestors' t (u : stack)
    leftAncestors' t stack = t : stack


data Combinator = I | K | S | B | C | Y | P | R | ADD | SUB | MUL | DIV | REM | SUB1 | EQL | GEQ | ZEROP | 
                 IF | B' | C' | S' | T | 
                 BulkCom String Int
                 --S2 | B2 | C2 | S3 | B3 | C3 | S4
  deriving (Eq, Show)

fromString :: String -> Combinator
fromString "i"    = I
fromString "k"    = K
fromString "s"    = S
fromString "b"    = B
fromString "c"    = C
fromString "s'"   = S'
fromString "b'"   = B'
fromString "c'"   = C'
fromString "y"    = Y
fromString "p"    = P
fromString "r"    = R
fromString "+"    = ADD
fromString "sub"  = SUB
fromString "div"  = DIV
fromString "rem"  = REM
fromString "*"    = MUL
fromString "sub1" = SUB1
fromString "eql"  = EQL
fromString "geq"  = GEQ
fromString "is0"  = ZEROP
fromString "if"   = IF
fromString "B"    = B
fromString "C"    = C
fromString "S"    = S
fromString "T"    = T
-- fromString "S2"   = S2
-- fromString "B2"   = B2
-- fromString "C2"   = C2
-- fromString "S3"   = S3
-- fromString "B3"   = B3
-- fromString "C3"   = C3
-- fromString "S4"   = S4

fromString _c     = error $ "unknown combinator " ++ _c 

toCL :: Expr -> CL
toCL (Var s) = Com (fromString s)
toCL (Lam x e) = error "toCL: lambda expression not in normal form"
toCL (m `App` n) = toCL m :@ toCL n
toCL (Int i) = INT i