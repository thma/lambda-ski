module CCC.CompilerElliott
  ( Cat (..)
  , compile
  , absCCC
  , eval
  , evalTop
  , Value (..)
  ) where

import           Parser (Environment, Expr (..))

-- Untyped CCC terms -----------------------------------------------------------

data Cat
  = CVar String       -- free variable (eliminated by absCCC)
  | CId               -- id
  | CComp Cat Cat     -- f . g
  | CFst | CSnd       -- projections
  | CFan Cat Cat      -- ⟨f, g⟩  (△)
  | CApply            -- apply (eval morphism)
  | CCurry Cat        -- curry f
  | CConst Integer    -- constant integer
  | CAdd | CSub | CMul
  | CEql | CLeq | CGeq
  | CFix Cat          -- fix (step)
  deriving (Show, Eq)

-- Compilation: Expr → Cat -----------------------------------------------------

compile :: Environment -> Expr -> Cat
compile _   (Int i)     = CConst i
compile env (Var x)     = resolveVar env x
compile env (App f a)
  | isFixOp env f       = compileFix env a
compile env (App f a)   = CComp CApply (CFan (compile env f) (compile env a))
compile env (Lam x body) = CCurry (absCCC x (compile env body))

resolveVar :: Environment -> String -> Cat
resolveVar env x = case lookup x env of
  Just e  -> compile env e
  Nothing -> resolvePrim x

resolvePrim :: String -> Cat
resolvePrim "+"     = CCurry CAdd
resolvePrim "-"     = CCurry CSub
resolvePrim "*"     = CCurry CMul
resolvePrim "sub"   = CCurry CSub
resolvePrim "sub1"  = CCurry (CComp CSub (CFan CSnd (CConst 1)))
resolvePrim "is0"   = CCurry (CComp CEql (CFan CSnd (CConst 0)))
resolvePrim "eql"   = CCurry CEql
resolvePrim "leq"   = CCurry CLeq
resolvePrim "geq"   = CCurry CGeq
resolvePrim "true"  = CSnd         -- Scott TRUE  = snd
resolvePrim "false" = CFst         -- Scott FALSE = fst
resolvePrim "if"    = CCurry (CCurry (CCurry ifBody))
  where
    -- context: (((env, sel), t), e)
    sel = CComp CSnd (CComp CFst CFst)     -- selector
    t   = CComp CSnd CFst                  -- then branch
    e   = CSnd                             -- else branch
    ifBody = CComp CApply (CFan sel (CFan e t))
resolvePrim name   = CVar name

-- Fixpoint detection ----------------------------------------------------------

isFixOp :: Environment -> Expr -> Bool
isFixOp env = go []
  where
    go _ (Int _)   = False
    go _ (Lam _ _) = False
    go _ (App _ _) = False
    go seen (Var name)
      | name == "y" || name == "fix" = True
      | name `elem` seen = False
      | otherwise = maybe False (go (name : seen)) (lookup name env)

compileFix :: Environment -> Expr -> Cat
compileFix env (Lam self body) =
  CFix (CCurry (absCCC self (compile env body)))
compileFix _ _ = error "fix expects a lambda step function"

-- Elliott's bracket abstraction -----------------------------------------------
--
-- The three core rules mirror SKI bracket abstraction:
--   [x] x       = snd                          (cf. I)
--   [x] e       = e . fst       (x not in e)   (cf. K)
--   [x] (f @ g) = apply . ⟨[x] f, [x] g⟩      (cf. S)
--
-- Plus structural rules for Cat constructors that arise from compilation.

absCCC :: String -> Cat -> Cat
-- Core rules:
absCCC x (CVar y) | x == y        = CSnd
absCCC x t | not (freeIn x t)     = CComp t CFst
absCCC x (CComp CApply (CFan f g)) =
  CComp CApply (CFan (absCCC x f) (absCCC x g))
-- Structural rules:
absCCC x (CFan f g)   = CFan (absCCC x f) (absCCC x g)
absCCC x (CComp f g)  = CComp (absCCC x f) (CFan (absCCC x g) CSnd)
absCCC x (CCurry f)   = CCurry (CComp (absCCC x f) assocR)
  where
    -- assocR : ((ctx, x), y) → ((ctx, y), x)
    assocR = CFan (CFan (CComp CFst CFst) CSnd) (CComp CSnd CFst)
absCCC x (CFix f)     = CFix (absCCC x f)
absCCC _ t             = error $ "absCCC: unexpected term " ++ show t

freeIn :: String -> Cat -> Bool
freeIn x (CVar y)    = x == y
freeIn x (CComp f g) = freeIn x f || freeIn x g
freeIn x (CFan f g)  = freeIn x f || freeIn x g
freeIn x (CCurry f)  = freeIn x f
freeIn x (CFix f)    = freeIn x f
freeIn _ _           = False

-- Untyped interpreter ---------------------------------------------------------

data Value
  = VInt Integer
  | VPair Value Value
  | VFun (Value -> Value)

instance Show Value where
  show (VInt i)    = show i
  show (VPair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
  show (VFun _)    = "<fun>"

vUnit :: Value
vUnit = VPair (VInt 0) (VInt 0)

evalTop :: Cat -> Integer
evalTop cat = case eval cat vUnit of
  VInt i -> i
  v      -> error $ "evalTop: expected integer, got " ++ show v

eval :: Cat -> Value -> Value
eval CId         v              = v
eval (CComp f g) v              = eval f (eval g v)
eval CFst        (VPair a _)    = a
eval CSnd        (VPair _ b)    = b
eval (CFan f g)  v              = VPair (eval f v) (eval g v)
eval CApply      (VPair f x)    = applyVal f x
eval (CCurry f)  v              = VFun (\x -> eval f (VPair v x))
eval (CConst i)  _              = VInt i
eval CAdd        (VPair (VInt a) (VInt b)) = VInt (a + b)
eval CSub        (VPair (VInt a) (VInt b)) = VInt (a - b)
eval CMul        (VPair (VInt a) (VInt b)) = VInt (a * b)
eval CEql        (VPair (VInt a) (VInt b)) = scottBool (a == b)
eval CLeq        (VPair (VInt a) (VInt b)) = scottBool (a <= b)
eval CGeq        (VPair (VInt a) (VInt b)) = scottBool (a >= b)
eval (CFix step) v              = let rec = VFun (\x -> eval (CFix step) x)
                                  in eval step (VPair rec v)
eval t           v              = error $ "eval: stuck on " ++ show t ++ " with " ++ show v

scottBool :: Bool -> Value
scottBool True  = VFun (\_ -> VFun id)        -- TRUE = snd: λt e. e
scottBool False = VFun (\x -> VFun (const x)) -- FALSE = fst: λt e. t

applyVal :: Value -> Value -> Value
applyVal (VFun f) x = f x
applyVal v        _ = error $ "applyVal: not a function: " ++ show v
