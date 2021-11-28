module LambdaToSKI
(
  compile
)
where

import Data.List
import Parser (Environment, Expr(..), parseEnvironment)  
import Text.Parsec (ParseError)

type Error = String

babs :: Environment -> Expr -> Expr
babs env (Lam x e)
  | Var "i" :@ x <- t                            = t
  | Var "s" :@ Var"k" :@ _ <- t                  = Var "s" :@ Var "k"
  | x `notElem` fv [] t                          = Var "k" :@ t
  | Var y <- t, x == y                           = Var "i" -- Var "s" :@  Var "k" :@ Var "k"
  | m :@ Var y <- t, x == y, x `notElem` fv [] m = m
  | Var y :@ m :@ Var z <- t, x == y, x == z     = babs env $ Lam x $ Var "s" :@ Var "s" :@ Var "k" :@ Var x :@ m
  | m :@ (n :@ l) <- t, isComb m, isComb n       = babs env $ Lam x $ Var "s" :@ Lam x m :@ n :@ l
  | (m :@ n) :@ l <- t, isComb m, isComb l       = babs env $ Lam x $ Var "s" :@ m :@ Lam x l :@ n
  | (m :@ l) :@ (n :@ l') <- t,
     l `noLamEq` l', isComb m, isComb n          = babs env $ Lam x $ Var "s" :@ m :@ n :@ l
  | m :@ n <- t                                  = Var "s" :@ babs env (Lam x m) :@ babs env (Lam x n)
  where t = babs env e
babs env (Var s)
  | Just t <- lookup s env = babs env t
  | otherwise              = Var s
babs env (m :@ n)          = babs env m :@ babs env n
babs env x                 = x

fv :: [String] -> Expr -> [String]
fv vs (Var s) | s `elem` vs = []
              | otherwise   = [s]
fv vs (x :@ y)              = fv vs x `union` fv vs y
fv vs (Lam s f)             = fv (s:vs) f
fv vs _                     = vs


isComb :: Expr -> Bool
isComb e = null $ fv [] e \\ ["s", "k"]

noLamEq :: Expr -> Expr -> Bool
noLamEq (Var x) (Var y) = x == y
noLamEq (a :@ b) (c :@ d) = a `noLamEq` c && b `noLamEq` d
noLamEq _ _ = False


opt :: Expr -> Expr
opt (Var "i" :@ n@(Int _n)) = n
opt ((Var "s" :@ e1) :@ (Var "k" :@ e2)) = (Var "c" :@ e1) :@ e2

--opt (x :@ y) = opt x :@ opt y
opt x = x

ropt :: Expr -> Expr
ropt expr =
  let expr' = opt expr
  in  if expr' == expr
        then expr
        else ropt expr'

{-
fun opt (sapp(sapp(scomb S,sapp(scomb K,e)),scomb I)) = (e : snode)
   |opt (sapp(sapp(scomb S,sapp(scomb K,e1)),sapp(scomb K,e2))) =
       sapp(scomb K,sapp(e1,e2))
   |opt (sapp(sapp(scomb S,sapp(scomb K,e1)),e2)) =
       sapp(sapp(scomb B,e1),e2)
   |opt (sapp(sapp(scomb S,e1),sapp(scomb K,e2))) =
       sapp(sapp(scomb C,e1),e2)
   |opt (sapp(e1,e2)) = sapp(opt e1,opt e2)
   |opt x = x;

-}

compile :: Environment-> Either Error Expr
compile env = case lookup "main" env of
  Nothing -> Left $ "main function missing in " ++ show env
  Just main -> Right $ compileToSKI env main

compileToSKI :: Environment -> Expr -> Expr
compileToSKI e = ropt . babs e

-- toSK :: String -> Either ParseError Expr
-- toSK s = do
--   env <- parseEnvironment s 
--   case lookup "main" env of
--     Nothing -> Left $ error "missing main function!"
--     Just t -> pure $ compileToSKI env t

-- getSK :: Either ParseError Expr -> Expr
-- getSK (Right exp) = exp
-- getSK (Left err)  = error $ show err

showSK :: Expr -> String
showSK (Var s)  = s ++ " "
showSK (x :@ y) = showSK x ++ showR y where
  showR (Var s) = s ++ " "
  showR _       = "(" ++ showSK y ++ ")"
showSK x        = show x ++ " "
