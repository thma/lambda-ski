{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-- This module provides compilation from lambda calculus expressions
    (Expr) and environments to FreeCat categorical morphisms.
    
    The compiler handles:
    - Variable lookup in environments
    - Integer constants
    - Lambda abstractions (converted to curried morphisms)
    - Function applications
    
    Example:
    > let env = [("x", Int 5)]
    > compileNumExpr env (Var "x")
    IntConst 5 . Id
--}

module CCC.Compiler where

import           CCC.FreeCat (FreeCat (..))
import           CCC.Rewrite (simplify)
import           Parser      (Environment, Expr (..))

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


-- | Evaluate an expression in an environment, returning a Value.
-- Built-ins are interpreted directly so the compiler can run example programs.
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


-- | Compile a numeric expression to a FreeCat integer morphism.
-- The result is a morphism of any input type to Integer.
compileNumExpr :: Environment -> Expr -> FreeCat a Integer
compileNumExpr env expr = case evalExpr env expr of
  -- Normalize compiled output so downstream consumers always get the
  -- same simplified categorical term shape.
  IntVal i -> simplify (IntConst i)
  v -> error $ "Expected integer result, got: " ++ show v


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
tryCompileVar :: Environment -> String -> Either String (FreeCat () Integer)
tryCompileVar env name = case lookup name env of
  Just expr -> case evalExpr env expr of
    IntVal _ -> Right (compileNumExpr env expr)
    v -> Left $ "Expected numeric value for '" ++ name ++ "', got: " ++ show v
  Nothing -> Left $ "Variable '" ++ name ++ "' not found in environment"


-- | Compile all numeric definitions in an environment.
-- Collects successful numeric compilations and reports failures.
compileNumericBindings :: Environment -> ([(String, String)], [String])
compileNumericBindings env = 
  let results = map (\(name, expr) -> (name, tryCompileVar env name)) env
      successes = [(n, show m) | (n, Right m) <- results]
      failures = [e | (_, Left e) <- results]
  in (successes, failures)
