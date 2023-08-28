{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module CLTerm
  (
    CL(..),
    Combinator(..),
    fromString
  )
  where

data CL = Com String | INT Integer | CL :@ CL
instance Show CL where
  showsPrec :: Int -> CL -> ShowS
  showsPrec p = \case
    Com s -> (if p > 0 then (' ':) else id) . (show s ++)
    INT i -> shows i
    t :@ u -> showParen (p > 0) $ shows t . showsPrec 1 u   

data Combinator = I | K | S | B | C | Y | P | ADD | SUB | MUL | DIV | REM | SUB1 | EQL | GEQ | ZEROP | IF | B' | C' | S'
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
fromString _c     = error $ "unknown combinator " ++ _c 