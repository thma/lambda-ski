# lambda-ski
Implementing a small functional language with a combinator based graph-reduction machine

[![Actions Status](https://github.com/thma/lambda-ski/workflows/Haskell%20CI/badge.svg)](https://github.com/thma/lambda-ski/actions)

## Overview

I took a λ-calculus parser from [https://crypto.stanford.edu/~blynn/lambda/sk.html](https://crypto.stanford.edu/~blynn/lambda/sk.html) 
and extended it to a tiny functional language which allows 
to write programs like the following:

```haskell
Y    = λf -> (λx -> x x)(λx -> f(x x))
fact = Y(λf n -> if (eq 0 n) 1 (* n (f (sub1 n))))
main = fact 1000
```

I'm then applying classic compilation from λ-calculus to combinatory logic combinators (S,K,I,B,C and Y) by bracket-abstraction and some optimization rules.

These combinator terms are then allocated into a graph data-structure.

This graph is then reduced by applying combinator graph-reduction. The inplace reduction of the graph is based on `STRef` mutable references. 

This part is based on a SML implementation I did some 30 years ago:
[https://github.com/thma/mg-book-sml-sources/blob/main/sml-sources/com41.sml](https://github.com/thma/mg-book-sml-sources/blob/main/sml-sources/com41.sml)

The basic ideas were taken from the classic [compiling functional languages](https://www.goodreads.com/book/show/3468677-compiling-functional-languages).

## representing λ-expressions

I'm aiming at a very rudimentry language that is basically just pure λ-calculus plus integers. 

```haskell
infixl 5 :@

data Expr
  = Expr :@ Expr
  | Var String
  | Int Integer
  | Lam String Expr
  deriving (Eq, Show)

type Environment = [(String, Expr)]
```

## The Parser



## Bracket abstraction

### Optimization

## Allocating a Graph with mutable references

## Performing graph-reduction




