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

import Parser (Expr (..), Environment)
import CCC.FreeCat (FreeCat (..))

-- | A value that can represent lambda-calculus terms in a typed setting.
-- This bridges untyped lambda expressions with typed categorical morphisms.
data Value 
  = IntVal Integer
  | FunVal (Value -> Value)
  | ExprVal Expr  -- for unevaluated expressions

instance Show Value where
  show (IntVal i) = show i
  show (FunVal _) = "<function>"
  show (ExprVal e) = show e


-- | Evaluate an expression in an environment, returning a Value.
-- This evaluation is lazy and preserves unevaluated lambda abstractions.
evalExpr :: Environment -> Expr -> Value
evalExpr env = \case
  Int i -> IntVal i
  
  Var name -> case lookup name env of
    Just e -> evalExpr env e
    Nothing -> error $ "Unbound variable: " ++ name
  
  -- Lambda abstractions become first-class values
  Lam param body -> FunVal $ \argVal -> 
    evalExpr ((param, valueToExpr argVal) : env) body
  
  -- Application: apply function value to argument  
  App f x -> case evalExpr env f of
    FunVal fn -> fn (evalExpr env x)
    v -> error $ "Cannot apply non-function value: " ++ show v
  
  where
    -- Convert a Value back to an Expr (lossy for functions)
    valueToExpr :: Value -> Expr
    valueToExpr (IntVal i) = Int i
    valueToExpr (FunVal _) = error "Cannot represent function as expression"
    valueToExpr (ExprVal e) = e


-- | Compile a numeric expression to a FreeCat integer morphism.
-- The result is a morphism of any input type to Integer.
compileNumExpr :: Environment -> Expr -> FreeCat a Integer
compileNumExpr env expr = case evalExpr env expr of
  IntVal i -> IntConst i
  v -> error $ "Expected integer result, got: " ++ show v


-- | Compile an expression, extracting environment variables to morphisms.
-- Returns a list of (name, morphism_string_representation) for inspection.
compileEnvironment :: Environment -> [(String, String)]
compileEnvironment env = map compileBinding env
  where
    compileBinding (name, expr) =
      case evalExpr env expr of
        IntVal i -> (name, "IntConst " ++ show i)
        FunVal _ -> (name, "<lambda function>")
        ExprVal e -> (name, show e)


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
