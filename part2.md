# Evaluating SKI combinators as native Haskell functions

[![Actions Status](https://github.com/thma/lambda-ski/workflows/Haskell-CI/badge.svg)](https://github.com/thma/lambda-ski/actions) <a href="https://github.com/thma/lambda-ski"><img src="https://thma.github.io/img/forkme.png" height="20" ></a>


## abstract

In this post I present an alternative approach to combinator-based implementation of functional languages that is significantly faster than classical graph-reduction based solutions.


## Introduction

In a [previous post](https://thma.github.io/posts/2021-12-27-Implementing-a-functional-language-with-Graph-Reduction.html) I presented a classical approach to implement a functional language with combinator based graph-reduction in Haskell.

The implementation was structured into three parts:

1. A parser for a tiny functional language based on the untyped λ-calculus.

2. A small compiler from λ-calculus to a fixed set of combinatory logic combinators (S,K,I,B,C and Y (aka. SICKBY)).

3. A graph-reduction engine which implements the combinator rewrite rules as an efficient graph reduction 

The basic idea is as follows:

Take a program like `main = (\x y -> x) 3 4` and compile it to a variable-free combinator expressions, in this case `K 3 4`.
Then apply the combinator reduction rules like `K x y = x` until normal-form is reached.

Graph-reduction is used as an efficient implementation technique for these reduction rules.
For example the reduction of `K` is implemented as follows:

```haskell
reduce :: Combinator -> LeftAncestorsStack s -> ST s ()
-- K x y = x
reduce K (p1 : p2 : _) = do
  (_K :@: xP) <- readSTRef p1
  xVal <- readSTRef xP
  writeSTRef p2 xVal
```
This code transforms the graph by directly writing the value `xVal`, stored in `xP` into the root node `p2`, as depicted below:

```haskell
p2:    @   ==>  3
      / \
p1:  @   4
    / \
   K   3
```

## Gaining a new perspective

The SICKBY combinators can be defined as ordinary Haskell functions:

```haskell
i x      = x         -- i = id
k x y    = x         -- k = const
s f g x  = f x (g x)
c f g x  = ((f x) g)
b f g x  = (f (g x))
y        = fix
```

So why don't we just use the Haskell native implementations of these combinators to reduce our expressions `implicitely`, 
rather than building our own graph reduction to `explicitly` reduce them?

It turns out that Matthew Naylor already wrote about this idea more than a decade ago in [The Monad Reader, issue 10](
https://wiki.haskell.org/wikiupload/0/0a/TMR-Issue10.pdf) (see also this [more recent coverage of the idea](https://smunix.github.io/kseo.github.io/posts/2016-12-30-write-you-an-interpreter.html)).

In the following section I will walk you through the details of this concept.

## Translating SICKBY expressions

In order to make use of haskell functions as combinators we'll first need a data structure that allows to include native functions in addition to the actual terms:


```haskell
import LambdaToSKI (Combinator (..), fromString)

-- | a compiled expression may be:
data CExpr
  = CComb Combinator       -- a known combinator symbol
  | CApp CExpr CExpr       -- an application (f x)
  | CFun (CExpr -> CExpr)  -- a native haskell function of type (CExpr -> CExpr)
  | CInt Integer           -- an integer

-- | declaring a show instance for CExpr
instance Show CExpr where
  show (CComb k)  = show k
  show (CApp a b) = "(" ++ show a ++ " " ++ show b ++ ")"
  show (CFun _f)  = "<function>"
  show (CInt i)   = show i
```

Translation from SICKBY terms (that is abstracted lambda expresssions) to `CExpr` is straightforward:

```haskell
-- | translating an abstracted lambda expression into a compiled expression
translate :: Expr -> CExpr
translate (fun :@ arg)   = CApp (translate fun) (translate arg)
translate (Int k)        = CInt k
translate (Var c)        = CComb (fromString c)
translate lam@(Lam _ _)  = error $ "lambdas should already be abstracted: " ++ show lam
```

1. Applications are translated by forming a `CApp` of the translated function and it's argument.
2. Integers are kept as is, just wrapped with a `CInt` constructor
3. After performing bracket abstraction any remaining `Var` must be a combinator. They are thus translated into a fixed combinator symbol; `fromString` looks up combinator symbols.
4. After bracket abstraction any remaining `Lam` expressions would be an error, so we treat it as such.

Please note that we do not use the `CFun` constructor in the translate stage. So right now the result of `translate` is just an ordinary data structure. Let's see an example:

```haskell
ghci> testSource = "main = (\\x y -> x) 3 4"
ghci> env = parseEnvironment testSource
ghci> compile env babs0
(((Var "s" :@ (Var "k" :@ Var "k")) :@ Var "i") :@ Int 3) :@ Int 4
ghci> cexpr = translate expr
ghci> cexpr
((((S (K K)) I) 3) 4)
```

Now it's time to do the real work. We will have to perform two essential transformations:
1. All combinators of the form `(CComb comb)` have to be replaced by the haskell functions implementing the combinator reduction rule.
2. All applications `(CApp fun arg)` have to be replaced by actual function application.
In our case we want apply functions of type `CExpr -> CExpr` that are wrapped by a `CFun` constructor. For this particular case we define an application operator `(!)` as follows:

    ```haskell
    -- | apply a CExpr of shape (CFun f) to argument x by evaluating (f x)
    infixl 0 !
    (!) :: CExpr -> CExpr -> CExpr
    (CFun f) ! x = f x
    ```


Both tasks are performed by the following `link` function:

```haskell
-- | a global environment of combinator definitions
type GlobalEnv = [(Combinator,CExpr)]

-- | "link" a compiled expression into Haskell native functions.
--   application terms will be transformed into (!) applications
--   combinator symbols will be replaced by their actual function definition
link :: GlobalEnv -> CExpr -> CExpr
link globals (CApp fun arg) = link globals fun ! link globals arg
link globals (CComb comb)   = fromJust $ lookup comb globals
link _globals expr          = expr
```

The global set of combinators is defined as follows:

```haskell
primitives :: GlobalEnv
primitives = let (-->) = (,) in
  [ I      --> CFun id
  , K      --> CFun (CFun . const)
  , S      --> CFun (\f -> CFun $ \g -> CFun $ \x -> f!x!(g!x))
  , B      --> CFun (\f -> CFun $ \g -> CFun $ \x -> f!(g!x))
  , C      --> CFun (\f -> CFun $ \g -> CFun $ \x -> f!x!g)
  , IF     --> CFun (\(CInt cond) -> CFun $ \thenExp -> CFun $ \elseExp -> 
                                        if cond == 1 then thenExp else elseExp)
  , Y      --> CFun (\(CFun f) -> fix f)
  , ADD    --> arith (+)
  , SUB    --> arith (-)
  , SUB1   --> CFun sub1
  , MUL    --> arith (*)
  , EQL    --> arith eql
  , GEQ    --> arith geq
  , ZEROP  --> CFun isZero
  ]

arith :: (Integer -> Integer -> Integer) -> CExpr
arith op = CFun $ \(CInt a) -> CFun $ \(CInt b) -> CInt (op a b)
```

As you can see, the combinators are implemented as `CFun` wrapped functions. So they bear some minor overhead for pattern matching the `CFun` constructor when using the `(!)` operator. But apart from that they are ordinary Haskell functions.

Trying out `link` in GHCi looks like follows:

```haskell
ghci> link primitives cexpr
3
```

So our initial expression `main = (\\x y -> x) 3 4` got translated into a haskell function applied to it's two arguments. As the function is fully saturated, the ghci implicit `show` request triggers its evaluation and we see the correct result `3` returned.

## We can do still better

I took the idea of having two passes, `translate` and `link` to transform the input SICKBY expressions verbatim from Matthew Naylor's paper. I think it's easier to explain the overall idea when breaking it down into these two separate steps. But it's perfectly possible do the transformation in one pass:

```haskell
-- | translate and link in one go
--   application terms will directly be transformed into (!) applications
--   combinator symbols will be replaced by their actual function definition
transLink :: GlobalEnv -> Expr -> CExpr
transLink globals (fun :@ arg)  = transLink globals fun ! transLink globals arg
transLink _globals (Int k)      = CInt k
transLink globals (Var c)       = fromJust $ lookup (fromString c) globals
transLink _globals l@(Lam _ _)  = error $ "lambdas should be abstracted already " ++ show l
```

In this case the `CExpr` type becomes even simpler, as no intermediate constructors are required for applications and combinators:

```haskell
-- | a compiled expression
data CExpr = 
    CFun (CExpr -> CExpr)
  | CInt Integer
```

## The good new and the good news

If you studied [my post on the roll your own graph-reduction idea]((https://thma.github.io/posts/2021-12-27-Implementing-a-functional-language-with-Graph-Reduction.html)) you will be amazed how much simpler the current approach is.

But it is also tremendously faster! 

I've assembled a set of [criterion micro-benchmarks for some typical recursive functions](https://github.com/thma/lambda-ski/blob/main/benchmark/ReductionBenchmarks.hs) on integers.

The table below compares the mean execution times for reducing the same program with GraphReduction and  with the "combinators as native functions". the third column gives the ratio between both execution times:


| | SICKBY GraphReduction [μs] |	SICKBY as functions [μs]	| ratio|
|-|-----------------------:|---------------------:|------:|
|[factorial](https://github.com/thma/lambda-ski/blob/main/test/factorial.ths) |    1273 |  24.92 | 51 |
|[fibonacci](https://github.com/thma/lambda-ski/blob/main/test/fibonacci.ths) |     484 |  50.70 | 10 |
|[ackermann](https://github.com/thma/lambda-ski/blob/main/test/ackermann.ths) |     386 |  16,88 | 23 |
|[gaussian sum](https://github.com/thma/lambda-ski/blob/main/test/gaussian.ths) | 1414 |  16,18 | 87 |
|[tak](https://github.com/thma/lambda-ski/blob/main/test/tak.ths) |          3204 |  75,69 | 42 |

For the fibonacci function the "combinators as native functions" approach is ten times faster, for the gaussian sum almost 90 times.

## Room for further improvements

It's interesting to see how the "combinators as native functions" execution performs in comparison to actual Haskell implementations of our five test functions. The Haskell native implementations can be found in the [benchmark definition](https://github.com/thma/lambda-ski/blob/main/benchmark/ReductionBenchmarks.hs).


| | SICKBY as functions [μs] |	Haskell native [μs]	| ratio|
|-|-----------------------:|---------------------:|------:|
|factorial |    24.92 | 4.743|   5 |
|fibonacci |    50.70 | 2.824|  18 |
|ackermann |    16,88 | 0.497|  34 |
|gaussian sum | 16,18 | 1.302|  12 |
|tak |          75,69 | 0.084| 903 |

For simple unary function like `factorial` and `gaussian sum` the native implementation is only 5 to 12 times faster. That's not bad for such a simple approach!

But for more complex functions like `fibonacci` and in particular for binary or ternary functions like `ackermann` and `tak` the performance is not that good. 

This is caused by the inefficient "code generation" of the classic bracket abstraction: [The output size grows quadratic](https://tromp.github.io/cl/LC.pdf) with internal complexity and number of variables. As each additional combinator or application will require additional execution time it's easy to see why a quadratic growth in combinator code size will drastically decrease performance. 
There have been many attempts to optimize bracket abstraction by [introducing additional combinators](https://www.cantab.net/users/antoni.diller/brackets/intro.html) and by [applying additional optimization rules](https://tromp.github.io/cl/LC.pdf).

I leave it as an exercise to the interested reader to improve the bracket abstraction rules applied here in order to sigificantly speed up both the graph-reduction as the "combinators as native functions" implementations.
