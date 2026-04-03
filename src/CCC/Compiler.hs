{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-- | Compilation from lambda calculus expressions (Expr) and environments
    to CatExpr categorical morphisms.
    
    The compiler handles:
    - Variable lookup in environments
    - Integer constants
    - Lambda abstractions (converted to curried morphisms)
    - Function applications
--}

module CCC.Compiler
  ( compileNumExpr,
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


-- | Compile a numeric expression to a CatExpr integer morphism.
-- The result is a morphism of any input type to Integer.
--
-- Compilation is a structural translation to CatExpr.
-- Unsupported constructs fail explicitly.
compileNumExpr :: Environment -> Expr -> CatExpr a Integer
compileNumExpr env expr =
  case compileIntExpr env expr of
    Right (Closed cat) -> simplify cat
    Left err           -> error ("Structural compilation failed: " ++ err)

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
  App (Var "y") stepExpr -> compileY env localEnv stepExpr
  Lam param body -> Right $ SFun $ \argVal -> compileExpr env ((param, argVal) : localEnv) body
  App f x -> do
    fVal <- compileExpr env localEnv f
    xVal <- compileExpr env localEnv x
    applySVal fVal xVal

applySVal :: SVal -> SVal -> Either String SVal
applySVal (SFun f) x = f x
applySVal _ _ = Left "Cannot apply non-function value"

data RVal c
  = RInt (CatExpr c Integer)
  | RBool (CatExpr c Bool)
  | RFun (RVal c -> Either String (RVal c))

data IntArgs input where
  OneArg :: IntArgs Integer
  MoreArgs :: IntArgs rest -> IntArgs (Integer, rest)

data SomeIntArgs where
  SomeIntArgs :: IntArgs input -> SomeIntArgs

compileY :: Environment -> [(String, SVal)] -> Expr -> Either String SVal
compileY env outerLocal = \case
  Lam fName stepExpr ->
    case collectLams stepExpr of
      (params, body) ->
        case mkIntArgs (length params) of
          Just (SomeIntArgs args) -> compileYGeneric env outerLocal args fName params body
          Nothing                 -> Left "y expects at least one integer argument"
  _ -> Left "y expects a lambda step function"

compileYGeneric ::
  Environment ->
  [(String, SVal)] ->
  IntArgs input ->
  String ->
  [String] ->
  Expr ->
  Either String SVal
compileYGeneric env outerLocal args fName params body = buildCurried args []
  where
    buildCurried :: IntArgs remaining -> [Closed Integer] -> Either String SVal
    buildCurried OneArg acc =
      Right $ SFun $ \case
        SInt arg -> do
          applied <- applyFix (acc ++ [arg])
          Right (SInt applied)
        _ -> Left "y expects Integer argument"
    buildCurried (MoreArgs rest) acc =
      Right $ SFun $ \case
        SInt arg -> buildCurried rest (acc ++ [arg])
        _ -> Left "y expects Integer argument"

    applyFix :: [Closed Integer] -> Either String (Closed Integer)
    applyFix actualArgs = do
      recFun <- buildRecFun args
      Closed paramTuple <- tupleFromArgs args actualArgs
      let local =
            (fName, recFun) :
            zipWith (\name projection -> (name, RInt projection)) params (argProjections args Snd) ++
            liftOuterLocal outerLocal
      stepBody <- compileRecExpr env outerLocal local body >>= expectRInt "Recursive body must compile to Integer"
      Right (Closed (Comp (Fix stepBody) paramTuple))

compileRecExpr :: Environment -> [(String, SVal)] -> [(String, RVal c)] -> Expr -> Either String (RVal c)
compileRecExpr env outerLocal local = \case
  Int i -> Right (RInt (IntConst i))
  Var name ->
    case lookup name local of
      Just v  -> Right v
      Nothing -> compileEnvVar name
  Lam param expr -> Right (RFun (\arg -> compileRecExpr env outerLocal ((param, arg) : local) expr))
  App (App (App (Var "if") cond) thenExpr) elseExpr -> do
    condV <- compileRecExpr env outerLocal local cond
    thenV <- compileRecExpr env outerLocal local thenExpr
    elseV <- compileRecExpr env outerLocal local elseExpr
    case (condV, thenV, elseV) of
      (RBool c, RInt t, RInt e) -> Right (RInt (Comp IfVal (fanC c (fanC t e))))
      _ -> Left "if expects (Bool, Int, Int)"
  App f x -> do
    fVal <- compileRecExpr env outerLocal local f
    xVal <- compileRecExpr env outerLocal local x
    applyRVal fVal xVal
  where
    compileEnvVar name =
      case lookup name env of
        Just expr -> do
          compiled <- compileExpr env outerLocal expr
          case sValToRVal compiled of
            Just v  -> Right v
            Nothing -> Left ("Unsupported environment value in recursive body: " ++ name)
        Nothing ->
          case compileRecBuiltin name of
            Just v  -> Right v
            Nothing -> Left ("Unbound variable: " ++ name)

    applyRVal (RFun f) x = f x
    applyRVal _ _        = Left "Cannot apply non-function value"

compileRecBuiltin :: String -> Maybe (RVal c)
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

rIntUnary :: (CatExpr c Integer -> CatExpr c Integer) -> RVal c
rIntUnary op = RFun $ \case
  RInt x -> Right (RInt (op x))
  _      -> Left "Expected integer argument"

rIntBin :: CatExpr (Integer, Integer) Integer -> RVal c
rIntBin op = RFun $ \left -> Right $ RFun $ \right ->
  case (left, right) of
    (RInt x, RInt y) -> Right (RInt (Comp op (fanC x y)))
    _                -> Left "Expected integer arguments"

rIntPred :: (CatExpr c Integer -> CatExpr c Bool) -> RVal c
rIntPred predicate = RFun $ \case
  RInt x -> Right (RBool (predicate x))
  _      -> Left "Expected integer argument"

rIntCmp :: CatExpr (Integer, Integer) Bool -> RVal c
rIntCmp op = RFun $ \left -> Right $ RFun $ \right ->
  case (left, right) of
    (RInt x, RInt y) -> Right (RBool (Comp op (fanC x y)))
    _                -> Left "Expected integer arguments"

sValToRVal :: SVal -> Maybe (RVal c)
sValToRVal (SInt (Closed c)) = Just (RInt c)
sValToRVal (SBool (Closed c)) = Just (RBool c)
sValToRVal (SFun _) = Nothing

liftOuterLocal :: [(String, SVal)] -> [(String, RVal c)]
liftOuterLocal [] = []
liftOuterLocal ((name, value) : rest) =
  case sValToRVal value of
    Just value' -> (name, value') : liftOuterLocal rest
    Nothing     -> liftOuterLocal rest

expectRInt :: String -> RVal c -> Either String (CatExpr c Integer)
expectRInt _ (RInt out) = Right out
expectRInt msg _        = Left msg

buildRecFun :: forall input. IntArgs input -> Either String (RVal (CatExpr input Integer, input))
buildRecFun args = build args []
  where
    build :: forall remaining. IntArgs remaining -> [CatExpr (CatExpr input Integer, input) Integer] -> Either String (RVal (CatExpr input Integer, input))
    build OneArg acc =
      Right $ RFun $ \case
        RInt arg -> do
          tupleExpr <- tupleFromExprs args (acc ++ [arg])
          Right (RInt (Comp Apply (fanC Fst tupleExpr)))
        _ -> Left "Recursive call expects Integer argument"
    build (MoreArgs rest) acc =
      Right $ RFun $ \case
        RInt arg -> build rest (acc ++ [arg])
        _ -> Left "Recursive call expects Integer argument"

argProjections :: IntArgs input -> CatExpr c input -> [CatExpr c Integer]
argProjections OneArg tupleExpr = [tupleExpr]
argProjections (MoreArgs rest) tupleExpr = Comp Fst tupleExpr : argProjections rest (Comp Snd tupleExpr)

tupleFromArgs :: forall input. IntArgs input -> [Closed Integer] -> Either String (Closed input)
tupleFromArgs args actualArgs = Right (Closed tupleExpr)
  where
    tupleExpr :: forall z. CatExpr z input
    tupleExpr =
      case tupleFromExprs args [expr | Closed expr <- actualArgs] of
        Right built -> built
        Left err    -> error err

tupleFromExprs :: IntArgs input -> [CatExpr c Integer] -> Either String (CatExpr c input)
tupleFromExprs OneArg [arg] = Right arg
tupleFromExprs (MoreArgs rest) (arg : restArgs) = do
  restTuple <- tupleFromExprs rest restArgs
  Right (fanC arg restTuple)
tupleFromExprs _ _ = Left "Incorrect recursive arity"

mkIntArgs :: Int -> Maybe SomeIntArgs
mkIntArgs n
  | n <= 0 = Nothing
mkIntArgs 1 = Just (SomeIntArgs OneArg)
mkIntArgs n = do
  SomeIntArgs rest <- mkIntArgs (n - 1)
  Just (SomeIntArgs (MoreArgs rest))

collectLams :: Expr -> ([String], Expr)
collectLams = go []
  where
    go params (Lam p expr) = go (params ++ [p]) expr
    go params expr         = (params, expr)

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

-- | Compile an expression, extracting environment variables to morphisms.
-- Returns a list of (name, morphism_string_representation) for inspection.
compileEnvironment :: Environment -> [(String, String)]
compileEnvironment env = map compileBinding env
  where
    compileBinding (name, expr) =
      case compileExpr env [] expr of
        Right (SInt (Closed cat)) -> (name, show (simplify cat))
        Right (SBool (Closed cat)) -> (name, show (simplify cat))
        Right (SFun _) -> (name, "<lambda function>")
        Left err -> (name, "<compile error: " ++ err ++ ">")


-- | Try to compile an environment variable to a numeric morphism.
-- Returns either the compiled morphism or an error message.
tryCompileVar :: Environment -> String -> Either String (CatExpr () Integer)
tryCompileVar env name =
  case lookup name env of
    Just expr ->
      case compileIntExpr env expr of
        Right (Closed cat) -> Right (simplify cat)
        Left err -> Left $ "Expected numeric value for '" ++ name ++ "', got: " ++ err
    Nothing -> Left $ "Variable '" ++ name ++ "' not found in environment"


-- | Compile all numeric definitions in an environment.
-- Collects successful numeric compilations and reports failures.
compileNumericBindings :: Environment -> ([(String, String)], [String])
compileNumericBindings env = 
  let results = map (\(name, _expr) -> (name, tryCompileVar env name)) env
      successes = [(n, show m) | (n, Right m) <- results]
      failures = [e | (_, Left e) <- results]
  in (successes, failures)
