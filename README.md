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

λ-expressions can be assigned to names in a top-level environment by using the `=` sign. those names may be referred to in other λ-expressions.
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

The parser module exports to function `parseEnvironmentEither` and `parseEnvironment`. The former is a total function returning an Either: `parseEnvironmentEither :: String -> Either ParseError Environment`, whereas the latter simply returns an `Environment` but may throw run-time errors.

The following [snippet](https://github.com/thma/lambda-ski/blob/main/app/Main.hs) demonstrates how a program is parsed into an Environment:


```haskell
testSource :: String
testSource =
       "Y    = λf -> (λx -> x x)(λx -> f(x x)) \n"
    ++ "fact = Y(λf n -> if (is0 n) 1 (* n (f (sub1 n)))) \n"
    ++ "main = fact 10 \n"

main = do
  let env = parseEnvironment testSource
  mapM_ print env
  putStrLn ""
```

This code results in the following output, which shows all `(String, Expr)` tuples in the environment:

```haskell
("Y",   Lam "f" (Lam "x" (Var "x" :@ Var "x") :@ Lam "x" (Var "f" :@ (Var "x" :@ Var "x"))))
("fact",Var "Y" :@ Lam "f" (Lam "n" (((Var "if" :@ (Var "is0" :@ Var "n")) :@ Int 1) :@ 
        ((Var "*" :@ Var "n") :@ (Var "f" :@ (Var "sub1" :@ Var "n"))))))
("main",Var "fact" :@ Int 10)
```

## Bracket abstraction

### Motivation

Of course it is possible to write interpreters that evaluate these λ-expression to normalform.
This is what any Lisp or Scheme eval/apply interpreter does at its core [(See a tiny example here)](http://www.sicpdistilled.com/section/4.1/).

One of the most problematic areas of these interpreters is the handling of variables. In order to provide static binding you will need closures that captures the current environment of variable bindings and thread them through the whole interpreter execution.

Language implemetors hav thus experimented with many ways to tackle this issue. One of the most influential ideas was to completely get rid of variables by abstracting them. 

The earliest version of this approach was [the SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus) invented by Haskell Curry and Moses Schönfinkel.

A λ-term that does not contain any free variables is said to be closed. Closed lambda terms are also called *combinators*. 

Schönfinkel and Curry found out that any closed λ-term can be rewritten in terms of three basic combinators I, K and S (in fact only *K* and *S* are essential, as *I* can be expressed as SKK):

<center>
<img src="https://latex.codecogs.com/gif.latex?\begin{array}{rcl}&space;I&space;&&space;=&space;&&space;\lambda&space;x.x&space;\\&space;K&space;&&space;=&space;&&space;\lambda&space;x.&space;\lambda&space;y.x&space;\\&space;S&space;&&space;=&space;&&space;\lambda&space;f.(\lambda&space;g.(\lambda&space;x.fx(gx)))&space;\end{array}" title="\begin{array}{rcl} I & = & \lambda x.x \\ K & = & \lambda x. \lambda y.x \\ S & = & \lambda f.(\lambda g.(\lambda x.fx(gx))) \end{array}" />
</center>

In Haskell these combinators can simply be defined as:

```haskell
i x = x
k x y = x
s f g x = f x (g x)
```



## The basic abstraction rules

The idea of bracket abstraction is to rewrite any closed λ-term in terms of I, K and S.
This recursive transformation is defined by the following equations:

<center>
<img src="https://latex.codecogs.com/gif.latex?\begin{array}{rcl}&space;\left&space;\lceil&space;\lambda&space;x.x&space;\right&space;\rceil&space;&&space;=&space;&&space;I&space;\\&space;\left&space;\lceil&space;\lambda&space;x.y&space;\right&space;\rceil&space;&&space;=&space;&&space;K&space;y&space;\\&space;\left&space;\lceil&space;\lambda&space;x.M&space;N&space;\right&space;\rceil&space;&&space;=&space;&&space;S&space;\left&space;\lceil&space;\lambda&space;x.M&space;\right&space;\rceil&space;\left&space;\lceil&space;\lambda&space;x.N&space;\right&space;\rceil&space;\end{array}" title="\begin{array}{rcl} \left \lceil \lambda x.x \right \rceil & = & I \\ \left \lceil \lambda x.y \right \rceil & = & K y \\ \left \lceil \lambda x.M N \right \rceil & = & S \left \lceil \lambda x.M \right \rceil \left \lceil \lambda x.N \right \rceil \end{array}" />
</center>

This can be implemented in Haskell as follows:

```haskell
-- | perform bracket abstraction on a lambda term and resolve free variables in the environment.
babs :: Environment -> Expr -> Expr
babs env (Lam x e)
  -- this implements the three equations for bracket abstraction given above
  | Var y <- t, x == y     = Var "i"
  | x `notElem` fv [] t    = Var "k" :@ t
  | m :@ n <- t            = Var "s" :@ babs env (Lam x m) :@ babs env (Lam x n)
  where t = babs env e
babs env (Var s)
  -- this tries to resolve free variables in the environment env
  | Just t <- lookup s env = babs env t
  | otherwise              = Var s 
babs env  (m :@ n)         = babs env m :@ babs env n
babs _env x                = x

-- | compute the list of free variables of a lambda expression
fv :: [String] -> Expr -> [String]
fv vs (Var s) | s `elem` vs = []
              | otherwise   = [s]
fv vs (x :@ y)              = fv vs x `union` fv vs y
fv vs (Lam s f)             = fv (s:vs) f
fv vs _                     = vs
```

```haskell
(((Var "s" :@ (Var "k" :@ ((Var "s" :@ Var "i") :@ Var "i"))) :@ ((Var "s" :@ ((Var "s" :@ (Var "k" :@ Var "s")) :@ ((Var "s" :@ (Var "k" :@ Var "k")) :@ Var "i"))) :@ (Var "k" :@ ((Var "s" :@ Var "i") :@ Var "i")))) :@ ((Var "s" :@ (Var "k" :@ (Var "s" :@ ((Var "s" :@ ((Var "s" :@ (Var "k" :@ Var "if")) :@ ((Var "s" :@ (Var "k" :@ Var "is0")) :@ Var "i"))) :@ (Var "k" :@ Int 1))))) :@ ((Var "s" :@ (Var "k" :@ (Var "s" :@ ((Var "s" :@ (Var "k" :@ Var "*")) :@ Var "i")))) :@ ((Var "s" :@ ((Var "s" :@ (Var "k" :@ Var "s")) :@ ((Var "s" :@ (Var "k" :@ Var "k")) :@ Var "i"))) :@ (Var "k" :@ ((Var "s" :@ (Var "k" :@ Var "sub1")) :@ Var "i")))))) :@ Int 10



main = (λx -> (+ x 4)) 5

("main",Lam "x" ((Var "+" :@ Var "x") :@ Int 4) :@ Int 5)

((Var "s" :@ ((Var "s" :@ (Var "k" :@ Var "+")) :@ Var "i")) :@ (Var "k" :@ Int 4)) :@ Int 5

after optimization:

((Var "c" :@ ((Var "b" :@ Var "+") :@ Var "i")) :@ Int 4) :@ Int 5
```

### Optimization

## Allocating a Graph with mutable references

## Performing graph-reduction

## CCC

```haskell
absCL (\x -> p q) = s (\x -> p) (\x -> q)

absCCC (\x -> p q) = apply . ((\x -> p) △ (\x -> q))

absCCC (\x -> p q) = apply . ((△) (\x -> p) (\x -> q))

```



