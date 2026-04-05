{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Naive (explicit CatExpr) compilation — no NBE.
module CCC.CompilerNaive
  ( compileNumExprNaive,
  ) where

import           CCC.CatExpr (CatExpr (..))
import           CCC.Cat     (fanC)
import           CCC.Compiler (RVal (..), IntArgs (..), SomeIntArgs (..),
                               compileRecBuiltin, collectLams, mkIntArgs,
                               buildRecFun, argProjections, tupleFromExprs,
                               expectRInt)
import           Parser      (Environment, Expr (..))

-- | Naive CCC compilation: builds explicit CatExpr nodes directly.
-- Unlike compileNumExpr (which uses NBE with Haskell closures for
-- beta-reduction), this compiles into the RVal domain from a unit
-- context, producing explicit Comp/fanC/Apply nodes at every step.
--
-- For first-order integer programs (no higher-order function passing),
-- the output is identical to the NBE version — NBE only gains an
-- advantage for higher-order terms where it can beta-reduce at compile time.
compileNumExprNaive :: Environment -> Expr -> CatExpr () Integer
compileNumExprNaive env expr =
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
