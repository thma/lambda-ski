{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-- | Compilation from lambda calculus expressions (Expr) and environments
    to CatExpr categorical morphisms.

    Two compilation strategies are used:

    1. Normalization by Evaluation (NBE) for closed/non-recursive terms.
       Terms are interpreted as 'SVal' in a Haskell semantic domain.
       Lambdas become Haskell closures; application is Haskell application.
       This implicitly encodes the CCC abstraction rules

         absCCC (λx. x)   = id
         absCCC (λx. c)   = const c          (x ∉ fv c)
         absCCC (λx. p q) = apply ∘ (absCCC (λx.p) △ absCCC (λx.q))

       without building explicit Curry/Uncurry/Apply nodes —
       beta-reduction happens at the Haskell metalevel instead.

    2. Direct CatExpr construction for recursive Fix bodies.
       Inside a Fix step, 'RVal c' carries CatExpr nodes indexed by a
       fixed context c = (recursive_fn, input_tuple), making tuple
       projections and composition explicit.

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

-- A closed morphism: valid in any input context z, i.e. a global element z → a.
-- RankN quantification ensures the expression is truly context-independent.
newtype Closed a = Closed (forall z. CatExpr z a)

-- Semantic value domain for NBE compilation.
-- SInt/SBool wrap closed morphisms (constants in the CCC sense).
-- SFun is a Haskell-level function modelling an arrow without fixing a context type.
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

-- Core lambda-to-CCC correspondence implemented by compileExpr:
--
-- Lambda term           | CCC rule                             | Compiler mechanism
-- n                     | absCCC (\x. n) = IntConst n          | Int i -> Closed (IntConst i)
-- x                     | absCCC (\x. x) = id                  | Var p -> lookup p localEnv returns same SVal
-- y (free)              | absCCC (\x. y) = const y             | same lookup path, yielding a Closed morphism
-- \x. e                 | curry (absCCC (\(x,y). e))           | Lam p body -> SFun (\v -> compile body[p:=v])
-- f g                   | apply . (f' △ g')                    | applySVal (compile f) (compile g) at meta-level
-- a ⊕ b                 | op . (a' △ b')                       | Comp op (fanC (compile a) (compile b))
-- y (\f a1...an. b)     | Fix step . (v1 △ ... △ vn)           | compileYGeneric -> Fix with context c = (f, input)

compileExpr :: Environment -> [(String, SVal)] -> Expr -> Either String SVal
compileExpr env localEnv = \case
  -- n  ↦  IntConst n   (absCCC (λx. n) = const n)
  Int i -> Right (SInt (Closed (IntConst i)))
  -- x  ↦  the SVal bound to x
  --   absCCC (λx. x) = id    — the arg passed in is returned unchanged
  --   absCCC (λx. y) = const y  — a Closed morphism, independent of x
  Var name ->
    case lookup name localEnv of
      Just value -> Right value
      Nothing ->
        case lookup name env of
          Just expr -> compileExpr env localEnv expr
          Nothing   -> compileBuiltin name
  -- y is compiled structurally to Fix rather than treated as a generic function
  App (Var "y") stepExpr -> compileY env localEnv stepExpr
  -- λx. e  ↦  Haskell closure; beta-reduction is deferred to apply time (NBE)
  --   absCCC (λx. λy. e) = curry (absCCC (λ(x,y). e))
  Lam param body -> Right $ SFun $ \argVal -> compileExpr env ((param, argVal) : localEnv) body
  -- f g  ↦  (compile f) applied to (compile g) at the Haskell level
  --   absCCC (λx. p q) = apply ∘ (absCCC (λx.p) △ absCCC (λx.q))
  App f x -> do
    fVal <- compileExpr env localEnv f
    xVal <- compileExpr env localEnv x
    applySVal fVal xVal

-- NBE application at the Haskell metalevel.
-- Encodes  apply ∘ (compile f △ compile x)  without building CatExpr nodes;
-- the result CatExpr is produced directly by the Haskell closure inside SFun.
applySVal :: SVal -> SVal -> Either String SVal
applySVal (SFun f) x = f x
applySVal _ _ = Left "Cannot apply non-function value"

-- Context-indexed values for direct Fix-body compilation.
-- Unlike SVal (which abstracts over context via universal quantification),
-- RVal c carries CatExpr nodes for a fixed context c, so projections
-- Fst/Snd can be composed explicitly to construct the step morphism.
data RVal c
  = RInt (CatExpr c Integer)
  | RBool (CatExpr c Bool)
  | RFun (RVal c -> Either String (RVal c))

-- Encodes the arity shape of a recursive function as a type-level structure.
-- n arguments map to the right-nested tuple type (Integer, (Integer, … Integer)).
data IntArgs input where
  OneArg  :: IntArgs Integer
  MoreArgs :: IntArgs rest -> IntArgs (Integer, rest)

-- Existential wrapper for IntArgs, used when arity is determined at runtime.
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

    -- Encodes the Fix rule:
    --   y (λf a₁…aₙ. body) at inputs (v₁,…,vₙ)
    --     = Comp (Fix stepBody) (v₁ △ … △ vₙ)
    -- Context for stepBody is c = (CatExpr input Integer, input):
    --   f   ↦  buildRecFun → Apply ∘ fanC Fst (a₁ △ … △ aₙ)
    --   aᵢ  ↦  i-th projection of Snd (see argProjections)
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
  -- n  ↦  IntConst n
  Int i -> Right (RInt (IntConst i))
  -- x  ↦  the RVal projection bound to x (Snd, Fst∘Snd, Fst∘Snd∘Snd, …)
  Var name ->
    case lookup name local of
      Just v  -> Right v
      Nothing -> compileEnvVar name
  -- λx. e  ↦  Haskell closure (same NBE trick as compileExpr)
  Lam param expr -> Right (RFun (\arg -> compileRecExpr env outerLocal ((param, arg) : local) expr))
  -- if c t e  ↦  IfVal ∘ (compile c △ (compile t △ compile e))
  App (App (App (Var "if") cond) thenExpr) elseExpr -> do
    condV <- compileRecExpr env outerLocal local cond
    thenV <- compileRecExpr env outerLocal local thenExpr
    elseV <- compileRecExpr env outerLocal local elseExpr
    case (condV, thenV, elseV) of
      (RBool c, RInt t, RInt e) -> Right (RInt (Comp IfVal (fanC c (fanC t e))))
      _ -> Left "if expects (Bool, Int, Int)"
  -- f g  ↦  (compile f) `applyRVal` (compile g)
  -- RFun closures build Comp/fanC nodes, so apply ∘ (compile f △ compile g) emerges in output.
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

-- Encodes:  compile(a ⊕ b) = Comp op (fanC (compile a) (compile b))
-- i.e.  op ∘ (compile a △ compile b) :: CatExpr c Integer
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

-- Builds the RVal for the recursive function 'f' inside the Fix body.
-- Context is c = (CatExpr input Integer, input), so:
--   Fst :: c → CatExpr input Integer   (the step function itself)
--   Snd :: c → input                   (the argument tuple)
-- A recursive call f a₁…aₙ is:  Apply ∘ fanC Fst (a₁ △ … △ aₙ)
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

-- Extracts individual integer argument projections from a right-nested tuple.
-- For OneArg:    the tuple is the integer itself → [tupleExpr]
-- For MoreArgs:  first arg = Fst ∘ tuple,  rest = projections on Snd ∘ tuple
-- Called as 'argProjections args Snd', yielding: Snd, Fst∘Snd, Fst∘Snd∘Snd, …
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

-- Same rule in the closed (NBE) domain:
--   compile(a ⊕ b) = Comp op (fanC (compile a) (compile b))
-- Both arguments are Closed (∀z), so fanC yields a valid closed morphism.
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
