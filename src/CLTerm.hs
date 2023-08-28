{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module CLTerm
  (
    CL(..)
  )
  where

data CL = Com String | INT Integer | CL :@: CL
instance Show CL where
  showsPrec :: Int -> CL -> ShowS
  showsPrec p = \case
    Com s -> (if p > 0 then (' ':) else id) . (s++)
    INT i -> shows i
    t :@: u -> showParen (p > 0) $ shows t . showsPrec 1 u    