{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{-- | Compilation from lambda calculus expressions (Expr) and environments
    to CatExpr categorical morphisms.
    
    The compiler handles:
    - Variable lookup in environments
    - Integer constants
    - Lambda abstractions (converted to curried morphisms)
    - Function applications
--}

module CCC.Compiler
  ( Value,
    evalExpr,
    compileNumExpr,
    compileEnvironment,
    tryCompileVar,
    compileNumericBindings
  ) where

import           CCC.CatExpr (CatExpr (..))
import           CCC.Cat     (fanC)
import           CCC.Rewrite (simplify)
import           Parser      (Environment, Expr (..))

newtype Closed a = Closed (forall z. CatExpr z a)

data SVal
  = SInt (Closed Integer)
  | SBool (Closed Bool)
  | SFun (SVal -> Either String SVal)

-- | A value that can represent lambda-calculus terms in a typed setting.
-- This bridges untyped lambda expressions with typed categorical morphisms.
data Value 
  = IntVal Integer
  | BoolVal Bool
  | FunVal (Value -> Value)

instance Show Value where
  show (IntVal i) = show i
  show (BoolVal b) = show b
  show (FunVal _) = "<function>"


-- | Legacy evaluator for Expr.
-- Used by helper APIs and as a fallback when structural CatExpr compilation
-- does not yet support a construct (for example recursion via y).
evalExpr :: Environment -> Expr -> Value
evalExpr env = evalWith []
  where
    evalWith :: [(String, Value)] -> Expr -> Value
    evalWith localEnv = \case
      App (App (App (Var "if") cond) thenExpr) elseExpr ->
        case evalWith localEnv cond of
          BoolVal True  -> evalWith localEnv thenExpr
          BoolVal False -> evalWith localEnv elseExpr
          value         -> error $ "Expected boolean condition, got: " ++ show value
      Int i -> IntVal i

      Var name ->
        case lookup name localEnv of
          Just value -> value
          Nothing ->
            case lookup name env of
              Just expr -> evalWith localEnv expr
              Nothing   -> builtin name

      Lam param body -> FunVal $ \argVal ->
        evalWith ((param, argVal) : localEnv) body

      App f x ->
        case evalWith localEnv f of
          FunVal fn -> fn (evalWith localEnv x)
          value     -> error $ "Cannot apply non-function value: " ++ show value

    builtin :: String -> Value
    builtin "+" = intBinOp (+)
    builtin "-" = intBinOp (-)
    builtin "*" = intBinOp (*)
    builtin "/" = intBinOp div
    builtin "sub" = intBinOp (-)
    builtin "sub1" = intUnaryOp (subtract 1)
    builtin "is0" = intPred (== 0)
    builtin "eql" = intCompare (==)
    builtin "leq" = intCompare (<=)
    builtin "geq" = intCompare (>=)
    builtin "if" = error "if must be applied to three arguments"
    builtin "y" = FunVal fixValue
    builtin "true" = BoolVal True
    builtin "false" = BoolVal False
    builtin name = error $ "Unbound variable: " ++ name

    intUnaryOp :: (Integer -> Integer) -> Value
    intUnaryOp op = FunVal $ \case
      IntVal integer -> IntVal (op integer)
      other          -> error $ "Expected integer argument, got: " ++ show other

    intBinOp :: (Integer -> Integer -> Integer) -> Value
    intBinOp op = FunVal $ \left -> FunVal $ \right ->
      case (left, right) of
        (IntVal leftInt, IntVal rightInt) -> IntVal (op leftInt rightInt)
        _ -> error $ "Expected integer arguments, got: " ++ show left ++ " and " ++ show right

    intPred :: (Integer -> Bool) -> Value
    intPred predicate = FunVal $ \case
      IntVal integer -> BoolVal (predicate integer)
      other          -> error $ "Expected integer argument, got: " ++ show other

    intCompare :: (Integer -> Integer -> Bool) -> Value
    intCompare predicate = FunVal $ \left -> FunVal $ \right ->
      case (left, right) of
        (IntVal leftInt, IntVal rightInt) -> BoolVal (predicate leftInt rightInt)
        _ -> error $ "Expected integer arguments, got: " ++ show left ++ " and " ++ show right

    -- The source language uses Y for recursive function definitions.
    fixValue :: Value -> Value
    fixValue (FunVal step) = result
      where
        result = step result
    fixValue other = error $ "Expected function argument to y, got: " ++ show other


-- | Compile a numeric expression to a CatExpr integer morphism.
-- The result is a morphism of any input type to Integer.
--
-- Compilation first attempts a structural translation to CatExpr.
-- Unsupported constructs currently fall back to the legacy evaluator via Lift.
compileNumExpr :: Environment -> Expr -> CatExpr a Integer
compileNumExpr env expr =
  case compileIntExpr env expr of
    Right (Closed cat) -> simplify cat
    -- Keep old behavior for currently unsupported forms (notably recursion via y).
    Left _             -> simplify (Lift (\_ -> evalNumExpr env expr))

compileIntExpr :: Environment -> Expr -> Either String (Closed Integer)
compileIntExpr env expr = do
  value <- compileExpr env [] expr
  case value of
    SInt compiled -> Right compiled
    _             -> Left "Expected integer expression"

compileExpr :: Environment -> [(String, SVal)] -> Expr -> Either String SVal
compileExpr env localEnv = \case
  Int i -> Right (SInt (Closed (IntConst i)))
  Var name ->
    case lookup name localEnv of
      Just value -> Right value
      Nothing ->
        case lookup name env of
          Just expr -> compileExpr env localEnv expr
          Nothing   -> compileBuiltin name
  App (Var "y") stepExpr -> compileUnaryY env localEnv stepExpr
  Lam param body -> Right $ SFun $ \argVal -> compileExpr env ((param, argVal) : localEnv) body
  App f x -> do
    fVal <- compileExpr env localEnv f
    xVal <- compileExpr env localEnv x
    applySVal fVal xVal

applySVal :: SVal -> SVal -> Either String SVal
applySVal (SFun f) x = f x
applySVal _ _ = Left "Cannot apply non-function value"

type RecCtx = (CatExpr Integer Integer, Integer)

data RVal
  = RInt (CatExpr RecCtx Integer)
  | RBool (CatExpr RecCtx Bool)
  | RFun (RVal -> Either String RVal)

compileUnaryY :: Environment -> [(String, SVal)] -> Expr -> Either String SVal
compileUnaryY env outerLocal = \case
  Lam fName (Lam nName body) ->
    Right $ SFun $ \case
      SInt (Closed arg) -> do
        stepBody <- compileRecBody env outerLocal fName nName body
        Right (SInt (Closed (Comp (Fix stepBody) arg)))
      _ -> Left "y expects Integer argument"
  _ -> Left "Only unary y (\\f n. body) is structurally supported"

compileRecBody ::
  Environment ->
  [(String, SVal)] ->
  String ->
  String ->
  Expr ->
  Either String (CatExpr RecCtx Integer)
compileRecBody env outerLocal fName nName body = do
  compiled <- go initialLocal body
  case compiled of
    RInt out -> Right out
    _        -> Left "Recursive body must compile to Integer"
  where
    initialLocal =
      [ (fName, RFun recCall),
        (nName, RInt Snd)
      ] ++ mapMaybeS outerLocal

    recCall :: RVal -> Either String RVal
    recCall (RInt x) = Right (RInt (Comp Apply (fanC Fst x)))
    recCall _        = Left "Recursive call expects Integer argument"

    go :: [(String, RVal)] -> Expr -> Either String RVal
    go local = \case
      Int i -> Right (RInt (IntConst i))
      Var name ->
        case lookup name local of
          Just v  -> Right v
          Nothing -> compileEnvVar name
      Lam param expr -> Right (RFun (\arg -> go ((param, arg) : local) expr))
      App (App (App (Var "if") cond) thenExpr) elseExpr -> do
        condV <- go local cond
        thenV <- go local thenExpr
        elseV <- go local elseExpr
        case (condV, thenV, elseV) of
          (RBool c, RInt t, RInt e) -> Right (RInt (Comp IfVal (fanC c (fanC t e))))
          _ -> Left "if expects (Bool, Int, Int)"
      App f x -> do
        fVal <- go local f
        xVal <- go local x
        applyRVal fVal xVal

    compileEnvVar :: String -> Either String RVal
    compileEnvVar name =
      case lookup name env of
        Just expr -> do
          compiled <- compileExpr env outerLocal expr
          case compiled of
            SInt (Closed c)  -> Right (RInt c)
            SBool (Closed c) -> Right (RBool c)
            _                -> Left ("Unsupported environment value in recursive body: " ++ name)
        Nothing ->
          case compileRecBuiltin name of
            Just v  -> Right v
            Nothing -> Left ("Unbound variable: " ++ name)

    compileRecBuiltin :: String -> Maybe RVal
    compileRecBuiltin "+" = Just (rIntBin Add)
    compileRecBuiltin "-" = Just (rIntBin Sub)
    compileRecBuiltin "*" = Just (rIntBin Mul)
    compileRecBuiltin "sub" = Just (rIntBin Sub)
    compileRecBuiltin "sub1" = Just (rIntUnary (\x -> Comp Sub (fanC x (IntConst 1))))
    compileRecBuiltin "is0" = Just (rIntPred (\x -> Comp Eql (fanC x (IntConst 0))))
    compileRecBuiltin "eql" = Just (rIntCmp Eql)
    compileRecBuiltin "leq" = Just (rIntCmp Leq)
    compileRecBuiltin "geq" = Just (rIntCmp Geq)
    compileRecBuiltin "true" = Just (RBool T)
    compileRecBuiltin "false" = Just (RBool F)
    compileRecBuiltin _ = Nothing

    applyRVal :: RVal -> RVal -> Either String RVal
    applyRVal (RFun f) x = f x
    applyRVal _ _        = Left "Cannot apply non-function value"

    rIntUnary :: (CatExpr RecCtx Integer -> CatExpr RecCtx Integer) -> RVal
    rIntUnary op = RFun $ \case
      RInt x -> Right (RInt (op x))
      _      -> Left "Expected integer argument"

    rIntBin :: CatExpr (Integer, Integer) Integer -> RVal
    rIntBin op = RFun $ \left -> Right $ RFun $ \right ->
      case (left, right) of
        (RInt x, RInt y) -> Right (RInt (Comp op (fanC x y)))
        _                -> Left "Expected integer arguments"

    rIntPred :: (CatExpr RecCtx Integer -> CatExpr RecCtx Bool) -> RVal
    rIntPred predicate = RFun $ \case
      RInt x -> Right (RBool (predicate x))
      _      -> Left "Expected integer argument"

    rIntCmp :: CatExpr (Integer, Integer) Bool -> RVal
    rIntCmp op = RFun $ \left -> Right $ RFun $ \right ->
      case (left, right) of
        (RInt x, RInt y) -> Right (RBool (Comp op (fanC x y)))
        _                -> Left "Expected integer arguments"

mapMaybeS :: [(String, SVal)] -> [(String, RVal)]
mapMaybeS [] = []
mapMaybeS ((name, value) : rest) =
  case value of
    SInt (Closed c)  -> (name, RInt c) : mapMaybeS rest
    SBool (Closed c) -> (name, RBool c) : mapMaybeS rest
    SFun _           -> mapMaybeS rest

compileBuiltin :: String -> Either String SVal
compileBuiltin "+" = Right (sIntBinOp Add)
compileBuiltin "-" = Right (sIntBinOp Sub)
compileBuiltin "*" = Right (sIntBinOp Mul)
compileBuiltin "sub" = Right (sIntBinOp Sub)
compileBuiltin "sub1" = Right (sIntUnaryOp (\x -> Comp Sub (fanC x (IntConst 1))))
compileBuiltin "is0" = Right (sIntPred (\x -> Comp Eql (fanC x (IntConst 0))))
compileBuiltin "eql" = Right (sIntCompare Eql)
compileBuiltin "leq" = Right (sIntCompare Leq)
compileBuiltin "geq" = Right (sIntCompare Geq)
compileBuiltin "if" = Right sIfFun
compileBuiltin "true" = Right (SBool (Closed T))
compileBuiltin "false" = Right (SBool (Closed F))
compileBuiltin "y" = Left "Recursion via y is not yet structurally compiled"
compileBuiltin "/" = Left "Division is not currently supported in CatExpr compilation"
compileBuiltin name = Left ("Unbound variable: " ++ name)

sIntUnaryOp :: (forall z. CatExpr z Integer -> CatExpr z Integer) -> SVal
sIntUnaryOp op = SFun $ \case
  SInt (Closed x) -> Right (SInt (Closed (op x)))
  _               -> Left "Expected integer argument"

sIntBinOp :: CatExpr (Integer, Integer) Integer -> SVal
sIntBinOp op = SFun $ \left -> Right $ SFun $ \right ->
  case (left, right) of
    (SInt (Closed x), SInt (Closed y)) -> Right (SInt (Closed (Comp op (fanC x y))))
    _                                   -> Left "Expected integer arguments"

sIntPred :: (forall z. CatExpr z Integer -> CatExpr z Bool) -> SVal
sIntPred predicate = SFun $ \case
  SInt (Closed x) -> Right (SBool (Closed (predicate x)))
  _               -> Left "Expected integer argument"

sIntCompare :: CatExpr (Integer, Integer) Bool -> SVal
sIntCompare op = SFun $ \left -> Right $ SFun $ \right ->
  case (left, right) of
    (SInt (Closed x), SInt (Closed y)) -> Right (SBool (Closed (Comp op (fanC x y))))
    _                                   -> Left "Expected integer arguments"

sIfFun :: SVal
sIfFun = SFun $ \cond -> Right $ SFun $ \thenVal -> Right $ SFun $ \elseVal ->
  case (cond, thenVal, elseVal) of
    (SBool (Closed c), SInt (Closed t), SInt (Closed e)) ->
      Right (SInt (Closed (Comp IfVal (fanC c (fanC t e)))))
    _ -> Left "if expects (Bool, Int, Int)"

-- | Evaluate an expression to an Integer value.
-- This is used by runtime interpretation of compiled Lift terms.
evalNumExpr :: Environment -> Expr -> Integer
evalNumExpr env expr =
  case evalExpr env expr of
    IntVal i -> i
    v        -> error $ "Expected integer result, got: " ++ show v


-- | Compile an expression, extracting environment variables to morphisms.
-- Returns a list of (name, morphism_string_representation) for inspection.
compileEnvironment :: Environment -> [(String, String)]
compileEnvironment env = map compileBinding env
  where
    compileBinding (name, expr) =
      case evalExpr env expr of
        IntVal i -> (name, "IntConst " ++ show i)
        BoolVal b -> (name, show b)
        FunVal _ -> (name, "<lambda function>")


-- | Try to compile an environment variable to a numeric morphism.
-- Returns either the compiled morphism or an error message.
tryCompileVar :: Environment -> String -> Either String (CatExpr () Integer)
tryCompileVar env name = case lookup name env of
  Just expr -> case evalExpr env expr of
    IntVal _ -> Right (compileNumExpr env expr)
    v -> Left $ "Expected numeric value for '" ++ name ++ "', got: " ++ show v
  Nothing -> Left $ "Variable '" ++ name ++ "' not found in environment"


-- | Compile all numeric definitions in an environment.
-- Collects successful numeric compilations and reports failures.
compileNumericBindings :: Environment -> ([(String, String)], [String])
compileNumericBindings env = 
  let results = map (\(name, _expr) -> (name, tryCompileVar env name)) env
      successes = [(n, show m) | (n, Right m) <- results]
      failures = [e | (_, Left e) <- results]
  in (successes, failures)
