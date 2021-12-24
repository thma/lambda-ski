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
-- | most basic bracket abstraction (plus resolution of free variables in the environment).
babs0 :: Environment -> Expr -> Expr
babs0 env (Lam x e) -- this clause implements the three basic equations for bracket abstraction
  | Var y <- t, x == y     = Var "i"
  | x `notElem` fv [] t    = Var "k" :@ t
  | m :@ n <- t            = Var "s" :@ babs0 env (Lam x m) :@ babs0 env (Lam x n)
  where t = babs0 env e
babs0 env (Var s) -- this clause resolves free variables by looking them up in the environment env
  | Just t <- lookup s env = babs0 env t
  | otherwise              = Var s
babs0 env  (m :@ n)        = babs0 env m :@ babs0 env n  -- this clause recurses into applications
babs0 _env x               = x                           -- returns anything else unchanged

-- | compute the list of free variables of a lambda expression
fv :: [String] -> Expr -> [String]
fv vs (Var s) | s `elem` vs = []
              | otherwise   = [s]
fv vs (x :@ y)              = fv vs x `union` fv vs y
fv vs (Lam s f)             = fv (s:vs) f
fv vs _                     = vs
```

Let's have a look at a simple example. first we parse a simple expression into a lambda-term:

```haskell
ghci> env = parseEnvironment "main = (λx -> + 4 x) 5\n"
ghci> env
[("main",Lam "x" ((Var "+" :@ Int 4) :@ Var "x") :@ Int 5)]
```

Next we apply bracket abstraction:

```
ghci> skiExpr = babs env (snd . head $ env)
ghci> skiExpr
((Var "s" :@ (Var "k" :@ (Var "+" :@ Int 4))) :@ Var "i") :@ Int 5
```

The result of bracket abstraction is still a lambda-term, but one where all `Lam`-expression have been eliminated.

### Optimization

Even from this simple example it is obvious that the SKI-combinator terms become larger than the original expressions. This will be an impediment to efficient implementation. So many different approaches have been conceived to mitigate this issue. 

The earliest solution, already suggested by Schönfinkel, is to introduce additional combinators B and C that cover specific patterns in the source code. Here are the reduction rules for B and C.

```haskell
C f g x = ((f x) g)
B f g x = (f (g x))
```

We could extend `babs` to cover B and C. But the most common way is to run a second optimization pass over the SKI-expression.

Here is is a simple example of such an optimization:

```haskell
opt :: Expr -> Expr
opt (Var "i" :@ n@(Int _n))                           = n
opt ((Var "s" :@ (Var "k" :@ e1)) :@ (Var "k" :@ e2)) = Var "k" :@ (e1 :@ e2)
opt ((Var "s" :@ e1) :@ (Var "k" :@ e2))              = (Var "c" :@ e1) :@ e2
opt ((Var "s" :@ (Var "k" :@ e1)) :@ e2)              = (Var "b" :@ e1) :@ e2
opt (x :@ y)                                          = opt x :@ opt y
opt x                                                 = x

ropt :: Expr -> Expr
ropt expr =
  let expr' = opt expr
  in  if expr' == expr
        then expr
        else case expr' of
          (x :@ y) -> ropt $ ropt x :@ ropt y
          _        -> ropt expr'
```

Let's try this out:

```haskell
ghci> optExpr = ropt skiExpr
ghci> optEpr
((Var "b" :@ (Var "+" :@ Int 4)) :@ Var "i") :@ Int 5
```

This looks much better than before. See [this project for a more in depth coverage of optimization techniques](https://crypto.stanford.edu/~blynn/lambda/logski.html). 
I'm also planning to write a separate blog post on this subtopic.

## Why graph reduction ?

So now that we have eliminated lambda abstractions from our lambda terms it should be straight forward to evaluate these expressions with a simple interpreter. 

Let's have a look at a simple example:

```haskell
sqr  = λx -> * x x
main = sqr (+ 3 2)
```

When we implement a strict interpreter with applicative-order semantics, `(+ 3 2)` will be computed first and the result bound to the variable `x` in the local environment and then the body of `sqr` will be evaluated in this environment. That's fine. but it's not normal-order reduction.

When implementing a lazy interpreter with normal-order semantics, we can not compute `(+ 3 2)` before binding it to `x`. Thus we will have to bind an un-evaluated *thunk* to `x`. We will also have to make sure that `x` is only evaluated when needed and only once, even when it is used at several places in the body of `sqr`. (See [these lecture notes for all the intricacies of this approach](https://academic.udayton.edu/saverioperugini/courses/cps343/lecture_notes/lazyevaluation.html))

Graph-reduction on the other hand, has some very interesting features:
- It maintains normal-order reduction (that is lazy evaluation)
- double evaluations of terms is avoided
- dealing with local environments, variable scope, etc. at run-time is avoided
- copying of argument data is significantly reduced as compared to eval/apply interpreters

Let's see this in action with our toy example. The above program can be transformed into the following SKI combinator term:

```haskell
((Var "s" :@ Var "*") :@ Var "i") :@ ((Var "+" :@ Int 3) :@ Int 2)
```

This term can be represented as a binary graph, where each application `:@` is represented as an `@` node, all combinators like `(Var "s")` are represented with 
Constructors like `S`, `I`, `MUL`, `ADD` and integer values like `Int 2` are just shown as numeric values like `2`:

```haskell 
          @                   
         / \              
        /   \               
       /     @          
      /     / \          
     /     @   2         
    @     / \     
   / \  ADD  3    
  @   I            
 / \             
S  MUL   
```

In the following diagram we follow the reduction of this graph:


```haskell 
          @                   @                   @                  @           25
         / \                 / \                 / \                / \
        /   \               /   \               /   \              /   |
       /     @             /     @             /     @            /   /
      /     / \           @     / \           @     / \          @   /
     /     @   2         / \   I   |         / \   I   |        / \ /
    @     / \           /   @ ––––/         /   5 ––––/        /   5
   / \  ADD  3         /   / \             /                  /
  @   I               /   @   2           /                  /
 / \                 /   / \             /                  /
S  MUL             MUL  ADD 3           MUL                MUL

Step 0             Step 1               Step 2             Step 3                Step 4
```

- **Step 0**: This is just the initial state of the graph as explained above.
  Please note that in this state the `S` is in *spine* position (i.e. the left-most ancestor of the root node) and *saturated* (i.e all three arguments of the combinator) are polulated, so according to the reduction rule `s f g x = f x (g x)` we expect to see a reduction `S MUL I (ADD 3 2) = MUL (ADD 3 2) (I (ADD 3 2))` in step 1.

- **Step 1**: As expected the first reduction step mutates the graph to represent `MUL (ADD 3 2) (I (ADD 3 2))`. Please note that both occurrences of `(ADD 3 2)` are represented by references to one and the same node. 

- **Step 2**: Now `MUL` is in *spine* position and *saturated*. But this time both arguments `(ADD 3 2)` and `I (ADD 3 2)` are not in normal-form and thus have to be reduced first before `MUL` can be executed. So first `(ADD 3 2)` is reduced to `5`. Please note that all references to this `5` 


## Allocating a Graph with mutable references

## Performing graph-reduction

## CCC

```haskell
absCL (\x -> p q) = s (\x -> p) (\x -> q)

absCCC (\x -> p q) = apply . ((\x -> p) △ (\x -> q))

absCCC (\x -> p q) = apply . ((△) (\x -> p) (\x -> q))

```



