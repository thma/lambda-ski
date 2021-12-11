# lambda-ski

[![Actions Status](https://github.com/thma/lambda-ski/workflows/Haskell%20CI/badge.svg)](https://github.com/thma/lambda-ski/actions)

## Abstract

Implementing a small functional language with a combinator based graph-reduction machine in Haskell.

I took a λ-calculus parser from [A Combinatory Compiler](https://crypto.stanford.edu/~blynn/lambda/sk.html) 
and extended it to cover a tiny functional language based on the untyped λ-calculus.

I'm then applying classic compilation from λ-calculus to combinatory logic combinators (S,K,I,B,C and Y) by bracket-abstraction and some optimization rules.

These combinator terms are then allocated into a graph data-structure.
Which  is then reduced by applying combinator graph-reduction. The destructive inplace reduction of the graph is made possible by using `STRef` mutable references. 

## Introduction

In my [last blog post](https://thma.github.io/posts/2021-04-04-Lambda-Calculus-Combinatory-Logic-and-Cartesian-Closed-Categories.html) I presented two ways of transforming λ-terms into variable free representations: 
- bracket abstraction to combinatory logic terms (SKI) and 
- bracket abstraction to terms of closed cartesian categories (CCC).

I demonstrated that both representations are equivalent as they imply the same reduction rules. 

My original intention was to extend an existing Haskell CCC implementation to a proof-of-concept implementation of a small functional language. I even promised to cover this in my next blog post.

I invested a lot of time in this idea but I failed to get it off the ground. [At least the code of these experiments has been preserved](https://github.com/thma/lambda-cat).

So I came back to writing a SKI graph-reduction as the backend of my language implementation. This is a well-worn path. The basic ideas were taken from the classic [compiling functional languages](https://www.goodreads.com/book/show/3468677-compiling-functional-languages) which dates back to 1988.

<!--
I already did a similar implementation in SML some 30 years ago:
[https://github.com/thma/mg-book-sml-sources/blob/main/sml-sources/com41.sml](https://github.com/thma/mg-book-sml-sources/blob/main/sml-sources/com41.sml)
-->

Fortunately, I did not fail this time! 
In the following I'm explaining my implementation approach. I'll also share some of my insights and talk about possible future extensions.

## representing λ-expressions

I'm aiming at a very rudimentry language that is basically just pure λ-calculus plus integers. Here is an example:

```haskell
Y    = λf . (λx . x x)(λx . f(x x))
fact = Y(\f n -> if (is0 n) 1 (* n (f (sub1 n))))
main = fact 10
```
As you can see it's possible to use classical λ-calculus notation `λx . x x` as well as Haskell syntax: `\x -> x x`. It's also possible to freely mix both styles.

λ-expressions can be assigned to names in a top-level environment by using the `=` sign. those names may be refrred to in other λ-expressions.
As of now recursive (also mutually recursive) references are not supported.

The `main` expression has a special meaning; it is interpreted as the entry point to a program.

With this knowledge at hand you will immediately recognize that the above program will compute the factorial of 10. Where `fact` is defined in a non-recursive way by means of the fixed-point combinator `Y`.

Expressions of this language are represented by the data type `Expr`:

```haskell
infixl 5 :@

data Expr
  = Expr :@ Expr
  | Var String
  | Int Integer
  | Lam String Expr
  deriving (Eq, Show)
```

The top-level environment which maps names to λ-Expressions is represented by the following type:

```haskell
type Environment = [(String, Expr)]
```

## The Parser

There is not much to see in [this area](https://github.com/thma/lambda-ski/blob/main/src/Parser.hs). It's just a simple Parsec based parser. Most of the code was taken from [A Combinatory Compiler](https://crypto.stanford.edu/~blynn/lambda/sk.html). I just added the parsing of Integers.

The parser module exports to function `parseEnvironmentEither` and `parseEnvironment`. The former is a total function returning an `Either ParseError Environment` whereas the latter simply returns an `Environment` but may throw run time errors.

https://github.com/thma/lambda-ski/blob/main/app/Main.hs


```haskell
main = do
  let env = parseEnvironment testSource
  mapM_ print env
  putStrLn ""
```

results in the following output:

```haskell
("Y",   Lam "f" (Lam "x" (Var "x" :@ Var "x") :@ Lam "x" (Var "f" :@ (Var "x" :@ Var "x"))))
("fact",Var "Y" :@ Lam "f" (Lam "n" (((Var "if" :@ (Var "is0" :@ Var "n")) :@ Int 1) :@ 
        ((Var "*" :@ Var "n") :@ (Var "f" :@ (Var "sub1" :@ Var "n"))))))
("main",Var "fact" :@ Int 10)
```

## Bracket abstraction

### Optimization

## Allocating a Graph with mutable references

## Performing graph-reduction

## CCC

```haskell
absCL (\x -> p q) = s (\x -> p) (\x -> q)

absCCC (\x -> p q) = apply . ((\x -> p) △ (\x -> q))

absCCC (\x -> p q) = apply . ((△) (\x -> p) (\x -> q))

```



