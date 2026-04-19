{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CCC.Compiler
  ( compileNumExpr
  ) where

import           CCC.CatExpr (CatExpr (..))
import           CCC.Cat     (fanC)
import           Parser      (Environment, Expr (..))

-- Public API ------------------------------------------------------------------

compileNumExpr :: Environment -> Expr -> CatExpr () Integer
compileNumExpr env expr =
  either (\e -> error ("Compilation failed: " ++ e)) id (compileIntExpr env expr)
  where
    compileIntExpr :: Environment -> Expr -> Either String (CatExpr () Integer)
    compileIntExpr env expr = compile env [] expr >>= expectInt

-- Core compilation ------------------------------------------------------------

compile :: forall c. Environment -> [(String, RVal c)] -> Expr -> Either String (RVal c)
compile env local = \case
  Int i -> Right (RInt (IntConst i))
  Var name ->
    case lookup name local of
      Just v  -> Right v
      Nothing -> resolveVar name
  App f x
    | isFixOp env local f -> compileFix env x
  Lam p body ->
    Right (RFun (\arg -> compile env ((p, arg) : local) body))
  App f x -> do
    fv <- compile env local f
    xv <- compile env local x
    apply fv xv
  where
    resolveVar name =
      case lookup name env of
        Just expr -> compile env local expr
        Nothing   -> maybe (Left $ "Unbound variable: " ++ name) Right (lookupBuiltin name)

    apply (RFun fn) x = fn x
    apply _ _          = Left "Cannot apply non-function value"

-- Fixpoint compilation --------------------------------------------------------

isFixOp :: Environment -> [(String, RVal c)] -> Expr -> Bool
isFixOp env local = go []
  where
    go _ (Int _)   = False
    go _ (Lam _ _) = False
    go _ (App _ _) = False
    go seen (Var name)
      | name `elem` map fst local = False
      | name == "y" || name == "fix" = True
      | name `elem` seen = False
      | otherwise = maybe False (go (name : seen)) (lookup name env)

compileFix :: forall c. Environment -> Expr -> Either String (RVal c)
compileFix env = \case
  Lam fName body ->
    let (params, bodyExpr) = collectLams body
    in case mkIntArgs (length params) of
         Just (SomeIntArgs args) -> Right $ compileFixBody env args fName params bodyExpr
         Nothing -> Left "fix expects at least one integer argument"
  _ -> Left "fix expects a lambda step function"

compileFixBody ::
  forall c input. Environment -> IntArgs input ->
  String -> [String] -> Expr -> RVal c
compileFixBody env args fName params body =
  curryIntArgs args $ \actualArgs -> do
    paramTuple <- tupleFromExprs args actualArgs
    let fixLocal = (fName, recFun args) :
          zipWith (\n p -> (n, RInt p)) params (projections args Snd)
    stepBody <- compile env fixLocal body >>= expectInt
    Right (RInt (Comp (Fix stepBody) paramTuple))

-- | Build a curried RVal that collects n integer arguments, then applies a continuation.
curryIntArgs :: forall c input. IntArgs input -> ([CatExpr c Integer] -> Either String (RVal c)) -> RVal c
curryIntArgs args k = go args []
  where
    go :: IntArgs r -> [CatExpr c Integer] -> RVal c
    go OneArg acc = RFun $ \case
      RInt a -> k (reverse (a : acc))
      _      -> Left "Expected Integer argument"
    go (MoreArgs rest) acc = RFun $ \case
      RInt a -> Right (go rest (a : acc))
      _      -> Left "Expected Integer argument"

-- | Build the recursive-call RVal inside a Fix body.
-- Context is (CatExpr input Integer, input), so Fst projects the step function
-- and a recursive call f a1...an becomes: Apply . fanC Fst (a1 ... an)
recFun :: forall input. IntArgs input -> RVal (CatExpr input Integer, input)
recFun args = curryIntArgs args $ \actualArgs -> do
  t <- tupleFromExprs args actualArgs
  Right (RInt (Comp Apply (fanC Fst t)))

-- IntArgs helpers -------------------------------------------------------------

data IntArgs input where
  OneArg   :: IntArgs Integer
  MoreArgs :: IntArgs rest -> IntArgs (Integer, rest)

data SomeIntArgs where
  SomeIntArgs :: IntArgs input -> SomeIntArgs

mkIntArgs :: Int -> Maybe SomeIntArgs
mkIntArgs n | n <= 0 = Nothing
mkIntArgs 1 = Just (SomeIntArgs OneArg)
mkIntArgs n = (\(SomeIntArgs r) -> SomeIntArgs (MoreArgs r)) <$> mkIntArgs (n - 1)

tupleFromExprs :: IntArgs input -> [CatExpr c Integer] -> Either String (CatExpr c input)
tupleFromExprs OneArg [a]              = Right a
tupleFromExprs (MoreArgs rest) (a : as) = fanC a <$> tupleFromExprs rest as
tupleFromExprs _ _                      = Left "Incorrect recursive arity"

projections :: IntArgs input -> CatExpr c input -> [CatExpr c Integer]
projections OneArg t          = [t]
projections (MoreArgs rest) t = Comp Fst t : projections rest (Comp Snd t)

collectLams :: Expr -> ([String], Expr)
collectLams (Lam p e) = let (ps, b) = collectLams e in (p : ps, b)
collectLams e         = ([], e)

-- RVal ------------------------------------------------------------------------

data RVal c
  = RInt (CatExpr c Integer)
  | RSel (CatExpr c (CatExpr (Integer, Integer) Integer))
  | RFun (RVal c -> Either String (RVal c))

expectInt :: RVal c -> Either String (CatExpr c Integer)
expectInt (RInt e) = Right e
expectInt _        = Left "Expected integer expression"

-- Builtins --------------------------------------------------------------------

lookupBuiltin :: String -> Maybe (RVal c)
lookupBuiltin "+"     = Just $ rBin RInt Add
lookupBuiltin "-"     = Just $ rBin RInt Sub
lookupBuiltin "*"     = Just $ rBin RInt Mul
lookupBuiltin "sub"   = Just $ rBin RInt Sub
lookupBuiltin "sub1"  = Just $ rUnary (\x -> RInt (Comp Sub (fanC x (IntConst 1))))
lookupBuiltin "is0"   = Just $ rUnary (\x -> RSel (Comp Eql (fanC x (IntConst 0))))
lookupBuiltin "eql"   = Just $ rBin RSel Eql
lookupBuiltin "leq"   = Just $ rBin RSel Leq
lookupBuiltin "geq"   = Just $ rBin RSel Geq
lookupBuiltin "true"  = Just $ RSel (Lift (const Snd))
lookupBuiltin "false" = Just $ RSel (Lift (const Fst))
lookupBuiltin "if"    = Just rIfFun
lookupBuiltin _       = Nothing

-- | Unary builtin: extracts one integer, applies f to produce any RVal.
rUnary :: (CatExpr c Integer -> RVal c) -> RVal c
rUnary f = RFun $ \case
  RInt x -> Right (f x)
  _      -> Left "Expected integer argument"

-- | Binary builtin: extracts two integers, applies op, wraps result.
-- wrap = RInt for arithmetic, RSel for comparisons.
rBin :: (CatExpr c r -> RVal c) -> CatExpr (Integer, Integer) r -> RVal c
rBin wrap op = RFun $ \l -> Right $ RFun $ \r ->
  case (l, r) of
    (RInt x, RInt y) -> Right (wrap (Comp op (fanC x y)))
    _                -> Left "Expected integer arguments"

-- Scott-encoded if: Apply . <selector, <elseVal, thenVal>>
rIfFun :: RVal c
rIfFun = RFun $ \case
  RSel sel -> Right $ RFun $ \case
    RInt t -> Right $ RFun $ \case
      RInt e -> Right (RInt (Comp Apply (fanC sel (fanC e t))))
      _      -> Left "if: else branch must be integer"
    _      -> Left "if: then branch must be integer"
  _        -> Left "if: condition must be a Scott boolean (selector)"