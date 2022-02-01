# Evaluating SKI combinators as native Haskell functions

[![Actions Status](https://github.com/thma/lambda-ski/workflows/Haskell%20CI/badge.svg)](https://github.com/thma/lambda-ski/actions) <a href="https://github.com/thma/lambda-ski"><img src="https://thma.github.io/img/forkme.png" height="20" ></a>


## abstract

## Introduction

In a [previous post](https://thma.github.io/posts/2021-12-27-Implementing-a-functional-language-with-Graph-Reduction.html) I presented a classical approach to implement a functional language with a combinator based graph-reduction machine in Haskell.

The implementation was structured into three parts:

1. A parser for a tiny functional language based on the untyped λ-calculus.

2. A small compiler from λ-calculus to a fixed set of combinatory logic combinators (S,K,I,B,C and Y (aka. SICKBY)).

3. A graph-reduction engine which implements the combinator rewrite rules as an efficient graph reduction 


The basic idea boils down to thew following:

Take a program like `main = (\x y -> x) 3 4` and compile it to variable-free combinator expressions, in this case `K 3 4`.
Then apply the combinator reduction rules like `K x y = x` until normal-form is reached.

Graph-reduction is used as an efficient implementation technique for these reduction rules in order to 
avoid term rewriting which would involve a lot of intermediary memory allocation.
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

So why don't we just reuse the Haskell native implementations of these combinators to reduce our expressions `implicitely`, 
rather than building our own graph reduction to `explicitly` reduce them?

It turns out that Matthew Naylor already wrote about this idea more than a decade ago in [The Monad Reader, issue 10](
https://wiki.haskell.org/wikiupload/0/0a/TMR-Issue10.pdf).

In the following section I will walk you through the details of this concept.

## Translating SICKBY expressions

In order to make use of haskell functions as combinators we'll first need a data structure that allows to include native functions in addition to the actual terms:


```haskell
-- | a compiled expression may be:
data CExpr
  = CComb Combinator       -- a known combinator symbol
  | CApp CExpr CExpr       -- an application (f x)
  | CFun (CExpr -> CExpr)  -- a native haskell function
  | CInt Integer           -- an integer
```

Translation from SICKBY terms (that is abstracted lambda expresssions) to `CExpr` is straightforward:

```haskell
-- | translating an abstracted lambda expression into a compiled expression
translate :: Expr -> CExpr
translate (fun :@ arg)   = CApp (translate fun) (translate arg)
translate (Int k)        = CInt k
translate (Var c)        = CComb (fromString c)
translate lam@(Lam _ _)  = error $ "lambdas should be abstracted already " ++ show lam
```

1. Applications are translated by forming a `CApp` of the tranlated function and it's argument.
2. Integers are kept as is
3. After abstraction any remaining `Var` must be a combinator. They are thus translated into a fixed combinator symbol.
4. After abstraction any remaining `Lam` expressions would be an error, so we treat it as such.

Please note that we do not construct any `CFun` instances in the translate stage. So right now the result of `translate` is just an ordinary data structure. Let's see any example:




show you how much this idea will reduce the complexity of our implementation.
It also turns 



 I will show you how to compile a program to a finite, fixed set of combinators (SKI), and then evaluate these combinators as normal Haskell function. This technique was introduced in Matthew Naylor’s Evaluating Haskell in Haskell.

The source code is available here.


## performance

```
SICKBY GraphReduction	SICKBY asunctions	Haskell native 	G/HHI	HHI/Native
factorial     1420   32,1   4,31   44   7
fibonacci      420   41,9   3,04   10  14
ackermann      450   20,9   0,405  22  52
gaussian sum  1420   20     1,65   71  12
tak           3610  112     0,781  32 143
              7320  226,9  10,186  32  22
```

## Related ideas

https://wiki.haskell.org/wikiupload/0/0a/TMR-Issue10.pdf

https://smunix.github.io/kseo.github.io/posts/2016-12-30-write-you-an-interpreter.html

