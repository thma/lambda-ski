module LambdaToSKI
  ( compileEither,
    compile,
    abstractToSKI,
    abstractSimple,
    abstractToCCC,
    babs,
    babs0,
    ropt,
    Combinator (..),
    fromString,
  )
where

import           Data.List (union, (\\))
import           Parser    (Environment, Expr (..))
import           CLTerm    (CL (..))

type Error = String

-- improved bracket abstraction according to https://tromp.github.io/cl/LC.pdf (section 3.2)
babs :: Environment -> Expr -> Expr
babs env (Lam x e)
  | Var "i" `App` _x <- t = t
  | Var "s" `App` Var "k" `App` _ <- t = Var "s" `App` Var "k"
  | x `notElem` fv [] t = Var "k" `App` t
  | Var y <- t, x == y = Var "i"
  | m `App` Var y <- t, x == y, x `notElem` fv [] m = m
  | Var y `App` m `App` Var z <- t, x == y, x == z = babs env $ Lam x $ Var "s" `App` Var "s" `App` Var "k" `App` Var x `App` m
  | m `App` (n `App` l) <- t, isComb m, isComb n = babs env $ Lam x $ Var "s" `App` Lam x m `App` n `App` l
--  | (m `App` n) `App` l <- t, isComb m, isComb l = babs env $ Lam x $ Var "s" `App` m `App` Lam x l `App` n -- this line is buggy (endless loop for tak)
  | (m `App` l) `App` (n `App` l') <- t,
    l `noLamEq` l',
    isComb m,
    isComb n =
    babs env $ Lam x $ Var "s" `App` m `App` n `App` l
  | m `App` n <- t = Var "s" `App` babs env (Lam x m) `App` babs env (Lam x n)
  where
    t = babs env e
babs env (Var s)
  | Just t <- lookup s env = babs env t
  | otherwise = Var s
babs env (m `App` n) = babs env m `App` babs env n
babs _env x = x

-- | most basic bracket abstraction (plus resolution of free variables in the environment).
babs0 :: Environment -> Expr -> Expr
babs0 env (Lam x e) -- this clause implements the three basic equations for bracket abstraction
  | Var y <- t, x == y = Var "i"
  | x `notElem` fv [] t = Var "k" `App` t
  | m `App` n <- t = Var "s" `App` babs0 env (Lam x m) `App` babs0 env (Lam x n)
  where
    t = babs0 env e
babs0 env (Var s) -- this clause resolves free variables by looking them up in the environment env
  | Just t <- lookup s env = babs0 env t
  | otherwise = Var s
babs0 env (m `App` n) = babs0 env m `App` babs0 env n -- this clause recurses into applications
babs0 _env x = x -- returns anything else unchanged

-- | find all free variables in a lambda expression
fv :: [String] -> Expr -> [String]
fv vs (Var s)
  | s `elem` vs = []
  | otherwise = [s]
fv vs (x `App` y) = fv vs x `union` fv vs y
fv vs (Lam s f) = fv (s : vs) f
fv vs _ = vs

isComb :: Expr -> Bool
isComb e = null $ fv [] e \\ ["s", "k", "i", "b", "c", "y", "s'", "b'", "c'"]

noLamEq :: Expr -> Expr -> Bool
noLamEq (Var x) (Var y)   = x == y
noLamEq (a `App` b) (c `App` d) = a `noLamEq` c && b `noLamEq` d
noLamEq _ _               = False

-- | optimizations according to Antoni Diller, Compiling Functional Languages, chapter 7
opt :: Expr -> Expr
opt (Var "i" `App` n@(Int _n))                           = n
opt ((Var "s" `App` (Var "k" `App` e1)) `App` (Var "k" `App` e2)) = Var "k" `App` (e1 `App` e2)
opt ((Var "s" `App` e1) `App` (Var "k" `App` e2))              = (Var "c" `App` e1) `App` e2
opt ((Var "s" `App` (Var "k" `App` e1)) `App` e2)              = (Var "b" `App` e1) `App` e2
opt ((Var "s" `App` ((Var "b" `App` p) `App` q)) `App` r)         = ((Var "s'" `App` p) `App` q) `App` r  -- Diller, p.98
opt ((Var "b" `App` (p `App` q) `App` r))                      = ((Var "b'" `App` p) `App` q) `App` r  -- Diller, p.98
opt ((Var "c" `App` ((Var "b" `App` p) `App` q)) `App` r)         = ((Var "c'" `App` p) `App` q) `App` r  -- Diller, p.98
opt (x `App` y)                                          = opt x `App` opt y
opt x                                                 = x

ropt :: Expr -> Expr
ropt expr =
  let expr' = opt expr
   in if expr' == expr
        then expr
        else case expr' of
          (x `App` y) -> ropt $ ropt x `App` ropt y
          _        -> ropt expr'

compileEither :: Environment -> (Environment -> Expr -> Expr) -> Either Error Expr
compileEither env abstractFun = case lookup "main" env of
  Nothing   -> Left $ "main function missing in " ++ show env
  Just main -> Right $ abstractFun env main

compile :: Environment -> (Environment -> Expr -> Expr) -> Expr
compile env abstractFun =
  case compileEither env abstractFun of
    Left err   -> error $ show err
    Right expr -> expr

toCL :: Expr -> CL
toCL (Var s) = Com s
toCL (Lam x e) = error "toCL: lambda expression not in normal form"
toCL (m `App` n) = toCL m :@: toCL n
toCL (Int i) = INT i


abstractToSKI :: Environment -> Expr -> Expr
abstractToSKI env = ropt . babs env

abstractSimple :: Environment -> Expr -> Expr
abstractSimple env = ropt . babs0 env

abstractToCCC :: Environment -> Expr -> Expr
abstractToCCC = cccAbs

cccAbs :: Environment -> Expr -> Expr
cccAbs env (Lam x e)
  | Var "id" `App` _x <- t = t
  | x `notElem` fv [] t = Var "const" `App` t
  | Var y <- t, x == y = Var "id"
  | m `App` Var y <- t, x == y, x `notElem` fv [] m = m
  | m `App` n <- t = Var "s" `App` cccAbs env (Lam x m) `App` cccAbs env (Lam x n)
  where
    t = cccAbs env e
cccAbs env (Var s)
  | Just t <- lookup s env = cccAbs env t
  | otherwise = Var s
cccAbs env (m `App` n) = cccAbs env m `App` cccAbs env n
cccAbs _env x = x

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