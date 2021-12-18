module LambdaToSKI
(
    compileEither
  , compile
  , abstractToSKI
  , abstractToCCC
  , babs
  , babs0
  , ropt
)
where

import Data.List (union, (\\))
import Parser (Environment, Expr(..))

type Error = String

babs :: Environment -> Expr -> Expr
babs env (Lam x e)
  | Var "i" :@ _x <- t                           = t
  | Var "s" :@ Var"k" :@ _ <- t                  = Var "s" :@ Var "k"
  | x `notElem` fv [] t                          = Var "k" :@ t
  | Var y <- t, x == y                           = Var "i"
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
babs env  (m :@ n)         = babs env m :@ babs env n
babs _env x                = x


-- | most basic bracket abstraction (plus resolution of free variables in the environment).
babs0 :: Environment -> Expr -> Expr
babs0 env (Lam x e) -- this clause implements the three basic equations for bracket abstraction
  | Var y <- t, x == y     = Var "i"
  | x `notElem` fv [] t    = Var "k" :@ t
  | m :@ n <- t            = Var "s" :@ babs0 env (Lam x m) :@ babs0 env (Lam x n)
  where t = babs0 env e
babs0 env (Var s) -- this clause resolves free variables by looking them up in the environment env
  | Just t <- lookup s env = babs0 env t
  | otherwise              = Var s
babs0 env  (m :@ n)        = babs0 env m :@ babs0 env n  -- this clause recurses into applications
babs0 _env x               = x                           -- returns anything else unchanged

-- | find all free variables in a lambda expression
fv :: [String] -> Expr -> [String]
fv vs (Var s) | s `elem` vs = []
              | otherwise   = [s]
fv vs (x :@ y)              = fv vs x `union` fv vs y
fv vs (Lam s f)             = fv (s:vs) f
fv vs _                     = vs


isComb :: Expr -> Bool
isComb e = null $ fv [] e \\ ["s", "k", "i", "b", "c", "y"]

noLamEq :: Expr -> Expr -> Bool
noLamEq (Var x) (Var y) = x == y
noLamEq (a :@ b) (c :@ d) = a `noLamEq` c && b `noLamEq` d
noLamEq _ _ = False


opt :: Expr -> Expr
opt (Var "i" :@ n@(Int _n))                           = n
opt ((Var "s" :@ (Var "k" :@ e1)) :@ (Var "k" :@ e2)) = Var "k" :@ (e1 :@ e2)
opt ((Var "s" :@ e1) :@ (Var "k" :@ e2))              = (Var "c" :@ e1) :@ e2
opt ((Var "s" :@ (Var "k" :@ e1)) :@ e2)              = (Var "b" :@ e1) :@ e2
opt (x :@ y)                                          = opt x :@ opt y
opt x                                                 = x

ropt :: Expr -> Expr
ropt expr =
  let expr' = opt expr
  in  if expr' == expr
        then expr
        else case expr' of
          (x :@ y) -> ropt $ ropt x :@ ropt y
          _        -> ropt expr'

compileEither :: Environment-> (Environment -> Expr -> Expr) -> Either Error Expr
compileEither env abstractFun = case lookup "main" env of
  Nothing -> Left $ "main function missing in " ++ show env
  Just main -> Right $ abstractFun env main

compile :: Environment -> (Environment -> Expr -> Expr) -> Expr
compile env abstractFun =
  case compileEither env abstractFun of
    Left err      -> error $ show err
    Right expr    -> expr  

abstractToSKI :: Environment -> Expr -> Expr
abstractToSKI main = ropt . babs main

abstractToCCC :: Environment -> Expr -> Expr
abstractToCCC = cccAbs

cccAbs :: Environment -> Expr -> Expr
cccAbs env (Lam x e)
  | Var "id" :@ _x <- t                          = t
  | x `notElem` fv [] t                          = Var "const" :@ t
  | Var y <- t, x == y                           = Var "id"
  | m :@ Var y <- t, x == y, x `notElem` fv [] m = m
  | m :@ n <- t                                  = Var "s" :@ cccAbs env (Lam x m) :@ cccAbs env (Lam x n)
  where t = cccAbs env e
cccAbs env (Var s)
  | Just t <- lookup s env = cccAbs env t
  | otherwise              = Var s
cccAbs env  (m :@ n)         = cccAbs env m :@ cccAbs env n
cccAbs _env x                = x

