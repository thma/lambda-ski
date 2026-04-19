# Compiling Lambda Expressions to Categorical Expressions

This document explains how `CCC.Compiler` translates untyped lambda calculus
expressions (`Expr`) into typed categorical morphisms (`CatExpr a b`), and how
those morphisms are then interpreted back into Haskell functions.

## Overview

The pipeline is:

```
Source text >── Parser ──▶ Environment + Expr >── Compiler ──▶ CatExpr () Integer >── Interpreter ──▶ () -> Integer
```

The compiler targets a **Closed Cartesian Category** (CCC) represented as a GADT.
Every compiled program is a morphism `CatExpr () Integer`: it takes no input and
produces an integer.

## Source language

`Expr` is a minimal untyped lambda calculus with integer literals:

```haskell
data Expr = App Expr Expr | Var String | Int Integer | Lam String Expr
```

Programs are written as a list of `name = expr` bindings (an `Environment`).
The compiler resolves `main` by looking it up in the environment and recursively
inlining variable references.

## Target language: CatExpr

`CatExpr a b` is a GADT representing morphisms in a CCC. The key constructors:

| Constructor | Type | Role |
|---|---|---|
| `Id` | `CatExpr a a` | Identity morphism |
| `Comp f g` | `CatExpr a c` | Composition $f \circ g$ |
| `Fst` | `CatExpr (a,b) a` | First projection |
| `Snd` | `CatExpr (a,b) b` | Second projection |
| `Dup` | `CatExpr a (a,a)` | Diagonal / fan-out |
| `Par f g` | `CatExpr (a,c) (b,d)` | Parallel product $f \times g$ |
| `IntConst n` | `CatExpr a Integer` | Constant morphism |
| `Add`, `Sub`, `Mul` | `CatExpr (a,a) a` | Arithmetic on pairs |
| `Eql`, `Leq`, ... | `CatExpr (a,a) (CatExpr (b,b) b)` | Comparisons (Scott-encoded) |
| `Apply` | `CatExpr (CatExpr a b, a) b` | Closed-category application |
| `Curry f` | `CatExpr a (CatExpr b c)` | Currying |
| `Lift f` | `CatExpr a b` | Embed a Haskell function |
| `Fix step` | `CatExpr a b` | Fixpoint combinator |

The helper `fanC f g = Par f g . Dup` (often written $\langle f, g \rangle$) duplicates the
input and applies two morphisms in parallel, producing a pair.

## Compilation strategy

### Intermediate representation: RVal

The compiler does not translate `Expr` directly to `CatExpr`. Instead it uses a
staging representation `RVal c`, parameterised by the CatExpr context type `c`:

```haskell
data RVal c
  = RInt (CatExpr c Integer)          -- a known-integer morphism
  | RSel (CatExpr c (CatExpr (Integer, Integer) Integer))  -- a Scott-encoded boolean
  | RFun (RVal c -> Either String (RVal c))                 -- a meta-level function
```

`RVal` is a **Normalisation by Evaluation (NBE)** trick: lambda abstractions stay
as Haskell functions (`RFun`) during compilation and are only reified into
`CatExpr` when their results are consumed. This avoids the need to explicitly
manage De Bruijn indices or perform substitution in the target AST.

### Core compilation: `compile`

```haskell
compile :: Environment -> [(String, RVal c)] -> Expr -> Either String (RVal c)
```

`compile` takes the global environment, a local scope of `RVal` bindings, and an
expression. It pattern-matches on the expression:

#### Integer literals

```
Int i  ⟶  RInt (IntConst i)
```

The integer $i$ becomes a constant morphism that ignores its input and returns $i$.

#### Variables

Looked up first in the local scope (returning an `RVal` directly), then in the
global environment (recursively compiling the referenced expression), and finally
in the builtin table.

#### Lambda abstractions

```
Lam p body  ⟶  RFun (\arg -> compile env ((p, arg) : local) body)
```

A lambda becomes an `RFun` — a Haskell closure that extends the local scope with
the parameter bound to whatever `RVal` is passed as the argument. The body is
compiled lazily when the function is applied.

This is the key NBE insight: function values are never directly encoded as
`CatExpr` terms. They exist only at the meta-level during compilation.

#### Application

For a normal (non-fixpoint) application `App f x`:

1. Compile `f` to get an `RVal`.
2. Compile `x` to get an `RVal`.
3. If `f` compiled to `RFun fn`, call `fn x` at the Haskell level.

This means beta-reduction happens at compile time. The result is a fully
normalised `CatExpr` with no residual lambdas — only compositions of primitive
morphisms.

### Worked example: addition

Consider compiling $(\lambda x\, y.\; + \; x \; y) \; 3 \; 5$.

1. `Lam "x" (Lam "y" (App (App (Var "+") (Var "x")) (Var "y")))` compiles to
   `RFun` (a Haskell closure).

2. Applying `RInt (IntConst 3)` binds `x = RInt (IntConst 3)` in the local scope
   and returns another `RFun`.

3. Applying `RInt (IntConst 5)` binds `y = RInt (IntConst 5)`.

4. The body `App (App (Var "+") (Var "x")) (Var "y")` is compiled:
   - `"+"` resolves to the builtin `rBin RInt Add`.
   - Applying the builtin to `RInt (IntConst 3)` then `RInt (IntConst 5)` yields:

   ```
   RInt (Comp Add (Comp (Par (IntConst 3) (IntConst 5)) Dup))
   ```

   which is $\texttt{Add} \circ \langle \texttt{IntConst 3}, \texttt{IntConst 5} \rangle$.

5. `expectInt` extracts the `CatExpr () Integer` from the `RInt`.

The interpreter evaluates this as $(+)(3, 5) = 8$.

## Builtins

Builtins are pre-built `RVal` values looked up by name:

| Name | RVal kind | Compiles to |
|---|---|---|
| `+`, `-`, `*`, `sub` | `RFun` → `RFun` → `RInt` | $\texttt{op} \circ \langle x, y \rangle$ |
| `sub1` | `RFun` → `RInt` | $\texttt{Sub} \circ \langle x, \texttt{IntConst 1} \rangle$ |
| `is0` | `RFun` → `RSel` | $\texttt{Eql} \circ \langle x, \texttt{IntConst 0} \rangle$ |
| `eql`, `leq`, `geq` | `RFun` → `RFun` → `RSel` | $\texttt{op} \circ \langle x, y \rangle$ |
| `true`, `false` | `RSel` | `Lift (const Snd)`, `Lift (const Fst)` |
| `if` | `RFun` → `RFun` → `RFun` → `RInt` | $\texttt{Apply} \circ \langle sel, \langle else, then \rangle \rangle$ |

### Scott-encoded booleans

Booleans are **not** a separate type. A boolean is a *selector morphism*
`CatExpr (b, b) b` that picks one element from a pair:

- **TRUE** = `Snd` — selects the second element (like the A combinator: $\lambda t\, e.\; e$)
- **FALSE** = `Fst` — selects the first element (like the K combinator: $\lambda t\, e.\; t$)

Comparison operators (`Eql`, `Leq`, ...) return these selectors. At the `RVal`
level they are tracked as `RSel` to distinguish them from plain integers.

Conditionals are compiled as:

$$\texttt{if}\;\mathit{cond}\;\mathit{then}\;\mathit{else} \;\longrightarrow\; \texttt{Apply} \circ \langle \mathit{selector}, \langle \mathit{elseVal}, \mathit{thenVal} \rangle \rangle$$

The selector is applied to a pair of alternatives. Note the argument order:
`then` is the second element (selected by `Snd`/TRUE) and `else` is the first
(selected by `Fst`/FALSE).

## Fixpoint compilation

Recursive functions defined with `y` or `fix` are compiled structurally using the
`Fix` combinator rather than unfolding the Y combinator at the lambda level.

### Detection

`isFixOp` checks whether the function position of an application is the fixpoint
combinator. It follows variable aliases through the environment (with cycle
detection) until it finds `"y"` or `"fix"`, or determines it is something else.

### The Fix combinator

`Fix` has type:

```haskell
Fix :: CatExpr (CatExpr a b, a) b -> CatExpr a b
```

It takes a *step function* that receives a pair of (self-reference, input) and
produces an output. The interpreter implements it as:

```haskell
interp (Fix step) = \a -> let rec = Fix step in interp step (rec, a)
```

### Compilation of $\texttt{y}\;(\lambda f.\;\lambda a_1 \ldots \lambda a_n.\;\mathit{body})\;x_1 \ldots x_n$

1. **Peel lambdas**: Extract the self-reference name $f$ and parameter names
   $[a_1, \ldots, a_n]$ from the step function.

2. **Build IntArgs**: The type-level structure of the parameter tuple is captured
   by a GADT:

   ```haskell
   data IntArgs input where
     OneArg   :: IntArgs Integer                          -- single arg
     MoreArgs :: IntArgs rest -> IntArgs (Integer, rest)  -- nested pair
   ```

   For arity 3, this builds `MoreArgs (MoreArgs OneArg)` representing the type
   `(Integer, (Integer, Integer))`.

   The existential is opened via CPS with `withIntArgs`:
   ```haskell
   withIntArgs :: Int -> (forall input. IntArgs input -> r) -> Maybe r
   ```

3. **Build the curried RVal**: `curryIntArgs` constructs an `RFun` that collects
   $n$ integer arguments one at a time, then passes them all to a continuation:

   ```haskell
   curryIntArgs :: IntArgs input -> ([CatExpr c Integer] -> Either String (RVal c)) -> RVal c
   ```

4. **Compile the step body**: Inside the continuation, the compiler:

   - Packs the collected arguments into a nested tuple using `tupleFromExprs`.
   - Builds a `recCall` binding for the self-reference $f$ — another curried
     `RVal` that collects arguments and produces $\texttt{Apply} \circ \langle \texttt{Fst}, \mathit{args} \rangle$.
     Here `Fst` projects the self-reference from the `(CatExpr a b, a)` context
     of the `Fix` body.
   - Builds parameter bindings by projecting from `Snd` of the fix context:
     $a_1 = \texttt{Comp Fst Snd}$, $a_2 = \texttt{Comp Fst (Comp Snd Snd)}$, etc.
   - Compiles the body expression with these bindings in scope.
   - Wraps everything as `Comp (Fix stepBody) paramTuple`.

### Worked example: factorial

Source:
```
fact = y(λf n. if (is0 n) 1 (* n (f (sub1 n))))
main = fact 5
```

Compilation of $\texttt{y}\;(\lambda f\, n.\;\mathit{body})\;5$:

1. $f$ = self-reference, $n$ = parameter, arity = 1, so `IntArgs` = `OneArg`.

2. The outer `curryIntArgs OneArg` collects one argument: `IntConst 5`.

3. Inside the Fix body (context type = `(CatExpr Integer Integer, Integer)`):
   - $f$ is bound to `recCall`: an `RFun` that produces
     $\texttt{Apply} \circ \langle \texttt{Fst}, \mathit{arg} \rangle$.
   - $n$ is bound to `RInt Snd` (project the integer input from the context).

4. The body `if (is0 n) 1 (* n (f (sub1 n)))` compiles to a `CatExpr` that
   uses `Eql`, `Sub`, `Mul`, `Apply`, `Fst`, `Snd` — all composed together.

5. The final result is `Comp (Fix stepBody) (IntConst 5)`.

The interpreter evaluates this by repeatedly calling `interp step (rec, n)`
until the base case is reached.

### Multi-argument recursion

For arity > 1 (e.g. `ack = y(λf n m. ...)`), arguments are packed into nested
tuples. With arity 2 the input type is `(Integer, Integer)` and the Fix context
is `(CatExpr (Integer, Integer) Integer, (Integer, Integer))`.

- $n$ projects as `Comp Fst Snd`
- $m$ projects as `Comp Snd Snd`
- A recursive call $f\;a\;b$ packs $(a, b)$ via `fanC` and applies via
  $\texttt{Apply} \circ \langle \texttt{Fst}, \langle a, b \rangle \rangle$.

## Interpretation

`CCC.Interpreter.interp` converts a `CatExpr a b` morphism into a Haskell
function `a -> b` by structural recursion:

```haskell
interp :: CatExpr a b -> (a -> b)
interp (Comp f g)   = interp f . interp g
interp Fst          = fst
interp Snd          = snd
interp (IntConst i) = const i
interp Add          = uncurry (+)
interp Apply        = uncurry interp
interp (Fix step)   = \a -> let rec = Fix step in interp step (rec, a)
...
```

The `Fix` case ties the knot: `rec` is the `Fix step` morphism itself, passed
back into the step function as its first argument. Haskell's laziness ensures
this only unfolds as far as needed.

## Summary of key design decisions

1. **NBE via RVal**: Lambdas stay as Haskell closures during compilation. This
   eliminates substitution, alpha-renaming, and De Bruijn bookkeeping.

2. **Scott-encoded booleans**: No boolean type in the target language.
   Comparisons produce selector morphisms; conditionals are just `Apply`.

3. **Structural Fix**: The Y combinator is intercepted and compiled to the `Fix`
   primitive, preserving recursion structure in the output rather than encoding it
   as self-application.

4. **Type-safe arity via IntArgs GADT**: The nested-tuple type of multi-argument
   fixpoints is tracked at the Haskell type level, ensuring `tupleFromExprs` and
   `projections` are type-correct despite the arity being determined at runtime.
