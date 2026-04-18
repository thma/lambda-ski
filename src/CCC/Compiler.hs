{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Naive (explicit CatExpr) compilation — no NBE.
module CCC.Compiler
  ( compileNumExpr,
    compileEnvironment,
    tryCompileVar,
    compileNumericBindings,
  ) where

import           CCC.CatExpr (CatExpr (..))
import           CCC.Cat     (fanC)
import           Parser      (Environment, Expr (..))

-- | Naive CCC compilation: builds explicit CatExpr nodes directly.
-- Unlike compileNumExpr (which uses NBE with Haskell closures for
-- beta-reduction), this compiles into the RVal domain from a unit
-- context, producing explicit Comp/fanC/Apply nodes at every step.
--
-- For first-order integer programs (no higher-order function passing),
-- the output is identical to the NBE version — NBE only gains an
-- advantage for higher-order terms where it can beta-reduce at compile time.
compileNumExpr :: Environment -> Expr -> CatExpr () Integer
compileNumExpr env expr =
  case compileNaive env [] expr of
    Right (RInt e) -> e
    Right _        -> error "Naive compilation: expected integer result"
    Left err       -> error ("Naive compilation failed: " ++ err)

-- Direct CatExpr compilation (no NBE).
-- Uses the same RVal machinery as compileRecExpr but starts from any context.
-- Builtins are RFun closures (primitives), user-level code builds explicit nodes.
--
-- Key difference from compileExpr (NBE):
--   NBE:   Lam → Haskell closure;  App → Haskell application (beta-reduces)
--   Naive: Lam → RFun closure;     App → RFun application (same node building)
-- For first-order programs, both produce identical CatExpr output because
-- builtins build the same Comp/fanC nodes. The difference appears only
-- with higher-order terms where NBE can beta-reduce at compile time.
compileNaive :: forall c. Environment -> [(String, RVal c)] -> Expr -> Either String (RVal c)
compileNaive env local = \case
  Int i -> Right (RInt (IntConst i))
  Var name ->
    case lookup name local of
      Just v  -> Right v
      Nothing -> compileNaiveVar name
  App fixHead stepExpr
    | isNaiveFixOp env local fixHead -> compileNaiveFix env stepExpr
  Lam param body ->
    Right (RFun (\arg -> compileNaive env ((param, arg) : local) body))
  App f x -> do
    fVal <- compileNaive env local f
    xVal <- compileNaive env local x
    applyNaiveVal fVal xVal
  where
    compileNaiveVar name =
      case lookup name env of
        Just expr -> compileNaive env local expr
        Nothing   -> case compileRecBuiltin name of
                       Just v  -> Right v
                       Nothing -> Left ("Unbound variable: " ++ name)

    applyNaiveVal (RFun fn) x = fn x
    applyNaiveVal _ _          = Left "Cannot apply non-function value"

isNaiveFixOp :: Environment -> [(String, RVal c)] -> Expr -> Bool
isNaiveFixOp env local = go []
  where
    go _ (Int _) = False
    go _ (Lam _ _) = False
    go _ (App _ _) = False
    go seen (Var name)
      | name `elem` map fst local = False
      | name == "y" || name == "fix" = True
      | name `elem` seen = False
      | otherwise =
          case lookup name env of
            Just expr -> go (name : seen) expr
            Nothing   -> False

compileNaiveFix :: forall c. Environment -> Expr -> Either String (RVal c)
compileNaiveFix env = \case
  Lam fName stepExpr ->
    case collectLams stepExpr of
      (params, body) ->
        case mkIntArgs (length params) of
          Just (SomeIntArgs args) -> compileNaiveFixGeneric env args fName params body
          Nothing                 -> Left "fix expects at least one integer argument"
  _ -> Left "fix expects a lambda step function"

compileNaiveFixGeneric ::
  forall c input.
  Environment ->
  IntArgs input ->
  String ->
  [String] ->
  Expr ->
  Either String (RVal c)
compileNaiveFixGeneric env args fName params body = buildCurried args []
  where
    buildCurried :: IntArgs remaining -> [CatExpr c Integer] -> Either String (RVal c)
    buildCurried OneArg acc =
      Right $ RFun $ \case
        RInt arg -> applyNaiveFix (acc ++ [arg])
        _        -> Left "fix expects Integer argument"
    buildCurried (MoreArgs rest) acc =
      Right $ RFun $ \case
        RInt arg -> buildCurried rest (acc ++ [arg])
        _        -> Left "fix expects Integer argument"

    applyNaiveFix :: [CatExpr c Integer] -> Either String (RVal c)
    applyNaiveFix actualArgs = do
      paramTuple <- tupleFromExprs args actualArgs
      recFun <- buildRecFun args
      let fixLocal =
            (fName, recFun) :
            zipWith (\name proj -> (name, RInt proj)) params (argProjections args Snd)
      stepBody <- compileNaive env fixLocal body >>= expectRInt "Recursive body must compile to Integer"
      Right (RInt (Comp (Fix stepBody) paramTuple))

compileNaiveIntExpr :: Environment -> Expr -> Either String (CatExpr () Integer)
compileNaiveIntExpr env expr =
  case compileNaive env [] expr of
    Right (RInt e) -> Right e
    Right _        -> Left "Expected integer expression"
    Left err       -> Left err

-- | Compile an expression, extracting environment variables to morphisms.
compileEnvironment :: Environment -> [(String, String)]
compileEnvironment env = map compileBinding env
  where
    compileBinding (name, expr) =
      case compileNaive env [] expr of
        Right (RInt cat)  -> (name, show cat)
        Right (RSel cat)  -> (name, show cat)
        Right (RFun _)    -> (name, "<lambda function>")
        Left err          -> (name, "<compile error: " ++ err ++ ">")

-- | Try to compile an environment variable to a numeric morphism.
tryCompileVar :: Environment -> String -> Either String (CatExpr () Integer)
tryCompileVar env name =
  case lookup name env of
    Just expr ->
      case compileNaiveIntExpr env expr of
        Right cat -> Right cat
        Left err  -> Left $ "Expected numeric value for '" ++ name ++ "', got: " ++ err
    Nothing -> Left $ "Variable '" ++ name ++ "' not found in environment"

-- | Compile all numeric definitions in an environment.
compileNumericBindings :: Environment -> ([(String, String)], [String])
compileNumericBindings env =
  let results = map (\(name, _expr) -> (name, tryCompileVar env name)) env
      successes = [(n, show m) | (n, Right m) <- results]
      failures = [e | (_, Left e) <- results]
  in (successes, failures)


---

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

compileRecBuiltin :: String -> Maybe (RVal c)
compileRecBuiltin name = builtinToRVal <$> lookupBuiltin name

-- Builtin descriptor: captures the shape of each primitive operation once.
-- Interpreted into both SVal (NBE) and RVal (direct) domains by
-- builtinToSVal and builtinToRVal respectively.
data Builtin where
  BinOp     :: CatExpr (Integer, Integer) Integer -> Builtin
  UnaryOp   :: (forall c. CatExpr c Integer -> CatExpr c Integer) -> Builtin
  Predicate :: (forall c. CatExpr c Integer -> CatExpr c (CatExpr (Integer, Integer) Integer)) -> Builtin
  CmpOp     :: (forall b. CatExpr (Integer, Integer) (CatExpr (b, b) b)) -> Builtin
  SelConst  :: CatExpr (Integer, Integer) Integer -> Builtin
  IfOp      :: Builtin

lookupBuiltin :: String -> Maybe Builtin
lookupBuiltin "+"     = Just (BinOp Add)
lookupBuiltin "-"     = Just (BinOp Sub)
lookupBuiltin "*"     = Just (BinOp Mul)
lookupBuiltin "sub"   = Just (BinOp Sub)
lookupBuiltin "sub1"  = Just (UnaryOp (\x -> Comp Sub (fanC x (IntConst 1))))
lookupBuiltin "is0"   = Just (Predicate (\x -> Comp Eql (fanC x (IntConst 0))))
lookupBuiltin "eql"   = Just (CmpOp Eql)
lookupBuiltin "leq"   = Just (CmpOp Leq)
lookupBuiltin "geq"   = Just (CmpOp Geq)
lookupBuiltin "true"  = Just (SelConst Snd)
lookupBuiltin "false" = Just (SelConst Fst)
lookupBuiltin "if"    = Just IfOp
lookupBuiltin _       = Nothing

builtinToRVal :: Builtin -> RVal c
builtinToRVal (BinOp op)     = rIntBin op
builtinToRVal (UnaryOp op)   = rIntUnary op
builtinToRVal (Predicate p)  = rIntToSel p
builtinToRVal (CmpOp op)     = rIntCmpSel op
builtinToRVal (SelConst sel)  = rSelConst sel
builtinToRVal IfOp            = rIfFun

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

expectRInt :: String -> RVal c -> Either String (CatExpr c Integer)
expectRInt _ (RInt out) = Right out
expectRInt msg _        = Left msg

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