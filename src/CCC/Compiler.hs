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
import           Parser      (Environment, Expr (..))

-- A closed morphism: valid in any input context z, i.e. a global element z → a.
-- RankN quantification ensures the expression is truly context-independent.
newtype Closed a = Closed (forall z. CatExpr z a)

-- Semantic value domain for NBE compilation.
-- SInt wraps closed integer morphisms (constants in the CCC sense).
-- SSel wraps closed selector morphisms (Scott-encoded booleans: Fst=FALSE, Snd=TRUE).
-- SFun is a Haskell-level function modelling an arrow without fixing a context type.
data SVal
  = SInt (Closed Integer)
  | SSel ClosedSel
  | SFun (SVal -> Either String SVal)

-- A closed selector morphism: a Scott-encoded boolean valid in any context.
-- Specialized to Integer since the compiler only handles integer programs.
newtype ClosedSel = ClosedSel (forall z. CatExpr z (CatExpr (Integer, Integer) Integer))


-- | Compile a numeric expression to a CatExpr integer morphism.
-- The result is a morphism of any input type to Integer.
--
-- Compilation is a structural translation to CatExpr.
-- Unsupported constructs fail explicitly.
compileNumExpr :: Environment -> Expr -> CatExpr a Integer
compileNumExpr env expr =
  case compileIntExpr env expr of
    Right (Closed cat) -> cat
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
-- fixOp (\f a1...an. b) | Fix step . (v1 △ ... △ vn)           | compileFixGeneric -> Fix with context c = (f, input)

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
  -- Structural recursion is detected by a fixpoint operator head (y/fix/alias)
  -- and compiled to CatExpr Fix instead of being interpreted as a regular function.
  App fixHead stepExpr
    | isFixOperator env localEnv fixHead -> compileFix env localEnv stepExpr
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
--
-- RSel carries a Scott-encoded boolean: a selector morphism CatExpr (a,a) a
-- represented as CatExpr c (CatExpr (Integer,Integer) Integer) in the fixed context.
data RVal c
  = RInt (CatExpr c Integer)
  | RSel (CatExpr c (CatExpr (Integer, Integer) Integer))
  | RFun (RVal c -> Either String (RVal c))

-- Encodes the arity shape of a recursive function as a type-level structure.
-- n arguments map to the right-nested tuple type (Integer, (Integer, … Integer)).
data IntArgs input where
  OneArg  :: IntArgs Integer
  MoreArgs :: IntArgs rest -> IntArgs (Integer, rest)

-- Existential wrapper for IntArgs, used when arity is determined at runtime.
data SomeIntArgs where
  SomeIntArgs :: IntArgs input -> SomeIntArgs

compileFix :: Environment -> [(String, SVal)] -> Expr -> Either String SVal
compileFix env outerLocal = \case
  Lam fName stepExpr ->
    case collectLams stepExpr of
      (params, body) ->
        case mkIntArgs (length params) of
          Just (SomeIntArgs args) -> compileFixGeneric env outerLocal args fName params body
          Nothing                 -> Left "fix expects at least one integer argument"
  _ -> Left "fix expects a lambda step function"

compileFixGeneric ::
  Environment ->
  [(String, SVal)] ->
  IntArgs input ->
  String ->
  [String] ->
  Expr ->
  Either String SVal
compileFixGeneric env outerLocal args fName params body = buildCurried args []
  where
    buildCurried :: IntArgs remaining -> [Closed Integer] -> Either String SVal
    buildCurried OneArg acc =
      Right $ SFun $ \case
        SInt arg -> do
          applied <- applyFix (acc ++ [arg])
          Right (SInt applied)
        _ -> Left "fix expects Integer argument"
    buildCurried (MoreArgs rest) acc =
      Right $ SFun $ \case
        SInt arg -> buildCurried rest (acc ++ [arg])
        _ -> Left "fix expects Integer argument"

    -- Encodes the Fix rule:
    --   fix (λf a₁…aₙ. body) at inputs (v₁,…,vₙ)
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

-- Detects whether an expression head denotes the structural fixpoint operator.
-- Recognized forms:
--   - direct builtins: y, fix
--   - environment aliases that resolve to those names
-- Local bindings shadow fixpoint names.
isFixOperator :: Environment -> [(String, SVal)] -> Expr -> Bool
isFixOperator env localEnv = go []
  where
    go _ (Int _) = False
    go _ (Lam _ _) = False
    go _ (App _ _) = False
    go seen (Var name)
      | name `elem` map fst localEnv = False
      | name == "y" || name == "fix" = True
      | name `elem` seen = False
      | otherwise =
          case lookup name env of
            Just expr -> go (name : seen) expr
            Nothing   -> False

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
compileRecBuiltin "is0" = Just (rIntToSel (\x -> Comp Eql (fanC x (IntConst 0))))
compileRecBuiltin "eql" = Just (rIntCmpSel Eql)
compileRecBuiltin "leq" = Just (rIntCmpSel Leq)
compileRecBuiltin "geq" = Just (rIntCmpSel Geq)
-- Scott-encoded booleans: TRUE = Snd (select second), FALSE = Fst (select first)
compileRecBuiltin "true" = Just (rSelConst Snd)
compileRecBuiltin "false" = Just (rSelConst Fst)
-- if selector thenVal elseVal = Apply ∘ ⟨selector, ⟨thenVal, elseVal⟩⟩
compileRecBuiltin "if" = Just rIfFun
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

-- Predicate returning a Scott-encoded boolean (selector morphism)
-- is0 n → Comp Eql (fanC n (IntConst 0)) :: CatExpr c (CatExpr (Integer,Integer) Integer)
rIntToSel :: (CatExpr c Integer -> CatExpr c (CatExpr (Integer, Integer) Integer)) -> RVal c
rIntToSel predicate = RFun $ \case
  RInt x -> Right (RSel (predicate x))
  _      -> Left "Expected integer argument"

-- Comparison returning a Scott-encoded boolean (selector morphism)
rIntCmpSel :: (forall b. CatExpr (Integer, Integer) (CatExpr (b, b) b)) -> RVal c
rIntCmpSel op = RFun $ \left -> Right $ RFun $ \right ->
  case (left, right) of
    (RInt x, RInt y) -> Right (RSel (Comp op (fanC x y)))
    _                -> Left "Expected integer arguments"

-- A constant Scott boolean (selector): true = Snd, false = Fst
rSelConst :: CatExpr (Integer, Integer) Integer -> RVal c
rSelConst sel = RSel (Lift (const sel))

-- Scott-encoded if: applies selector to pair of alternatives
-- if selector thenVal elseVal = Apply ∘ ⟨selector, ⟨elseVal, thenVal⟩⟩
-- Pair is (else, then) because TRUE=Snd selects second=then, FALSE=Fst selects first=else
-- This matches SICKBY convention: if = λc t e. c e t
rIfFun :: RVal c
rIfFun = RFun $ \case
  RSel sel -> Right $ RFun $ \case
    RInt t -> Right $ RFun $ \case
      RInt e -> Right (RInt (Comp Apply (fanC sel (fanC e t))))
      _      -> Left "if: else branch must be integer"
    _      -> Left "if: then branch must be integer"
  _        -> Left "if: condition must be a Scott boolean (selector)"

sValToRVal :: SVal -> Maybe (RVal c)
sValToRVal (SInt (Closed c)) = Just (RInt c)
sValToRVal (SSel (ClosedSel c)) = Just (RSel c)
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
compileBuiltin "is0" = Right (sIntToSel (\x -> Comp Eql (fanC x (IntConst 0))))
compileBuiltin "eql" = Right (sIntCmpSel Eql)
compileBuiltin "leq" = Right (sIntCmpSel Leq)
compileBuiltin "geq" = Right (sIntCmpSel Geq)
compileBuiltin "if" = Right sIfFun
-- Scott-encoded booleans: TRUE = Snd (select second), FALSE = Fst (select first)
compileBuiltin "true" = Right (SSel (ClosedSel (Lift (const Snd))))
compileBuiltin "false" = Right (SSel (ClosedSel (Lift (const Fst))))
compileBuiltin "y" = Left "Recursion via fixpoint operators is structurally compiled"
compileBuiltin "fix" = Left "Recursion via fixpoint operators is structurally compiled"
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

-- Predicate returning a Scott-encoded boolean (selector morphism)
sIntToSel :: (forall z. CatExpr z Integer -> CatExpr z (CatExpr (Integer, Integer) Integer)) -> SVal
sIntToSel predicate = SFun $ \case
  SInt (Closed x) -> Right (SSel (ClosedSel (predicate x)))
  _               -> Left "Expected integer argument"

-- Comparison returning a Scott-encoded boolean (selector morphism)
sIntCmpSel :: (forall b. CatExpr (Integer, Integer) (CatExpr (b, b) b)) -> SVal
sIntCmpSel op = SFun $ \left -> Right $ SFun $ \right ->
  case (left, right) of
    (SInt (Closed x), SInt (Closed y)) -> Right (SSel (ClosedSel (Comp op (fanC x y))))
    _                                   -> Left "Expected integer arguments"

-- Scott-encoded if: applies selector to pair of alternatives
-- if selector thenVal elseVal = Apply ∘ ⟨selector, ⟨elseVal, thenVal⟩⟩
-- Pair is (else, then) because TRUE=Snd selects second=then, FALSE=Fst selects first=else
-- This matches SICKBY convention: if = λc t e. c e t
sIfFun :: SVal
sIfFun = SFun $ \case
  SSel (ClosedSel sel) -> Right $ SFun $ \case
    SInt (Closed t) -> Right $ SFun $ \case
      SInt (Closed e) -> Right (SInt (Closed (Comp Apply (fanC sel (fanC e t)))))
      _               -> Left "if: else branch must be integer"
    _               -> Left "if: then branch must be integer"
  _                  -> Left "if: condition must be a Scott boolean (selector)"

-- | Compile an expression, extracting environment variables to morphisms.
-- Returns a list of (name, morphism_string_representation) for inspection.
compileEnvironment :: Environment -> [(String, String)]
compileEnvironment env = map compileBinding env
  where
    compileBinding (name, expr) =
      case compileExpr env [] expr of
        Right (SInt (Closed cat)) -> (name, show cat)
        Right (SSel (ClosedSel cat)) -> (name, show cat)
        Right (SFun _) -> (name, "<lambda function>")
        Left err -> (name, "<compile error: " ++ err ++ ">")


-- | Try to compile an environment variable to a numeric morphism.
-- Returns either the compiled morphism or an error message.
tryCompileVar :: Environment -> String -> Either String (CatExpr () Integer)
tryCompileVar env name =
  case lookup name env of
    Just expr ->
      case compileIntExpr env expr of
        Right (Closed cat) -> Right cat
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
