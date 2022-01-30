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
rather than `explicitly` with our home grown graph reduction?

It turns out that Matthew Naylor wrote about this approach more than a decade ago in [The Monad Reader, issue 10](
https://wiki.haskell.org/wikiupload/0/0a/TMR-Issue10.pdf).

In the following section I will walk through the details of this concept.



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

