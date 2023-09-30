# Optimizing bracket abstraction for Combinator based interpreters

## Abstract

In this post i will show how to significantly improve the performance of a combinator based interpreter by using an alternative abstraction algorithm. This algorithm is based on the paper [Optimizing bracket abstraction](https://okmij.org/ftp/tagless-final/ski.pdf) by Oleg Kiselyov and closely follows Ben Lynn's implementation of Kiselyov's algorithm in [his blog post](https://crypto.stanford.edu/~blynn/lambda/kiselyov.html).

I will also give some performance comparisons between the different approaches.

## Introduction

In previous blog posts i have shown how functional languages can be implemented using a small set of combinators. 

**The first post**, [Implementing a functional language with Graph Reduction](https://thma.github.io/posts/2021-12-27-Implementing-a-functional-language-with-Graph-Reduction.html) described an approach that sets up three major components:

- A parser for a tiny functional language based on the untyped λ-calculus.

- A compiler from λ-calculus to a fixed set of combinatory logic combinators (S,K,I,B,C and Y (aka. SICKBY)). This compiler uses traditional bracket abstraction algorithms to encode λ-terms as combinators.

- A graph-reduction engine which implements the combinator rewrite rules as an efficient graph reduction

**In the second post**, [Evaluating SKI combinators as native Haskell functions](https://thma.github.io/posts/2022-02-05-Evaluating-SKI-combinators-as-native-Haskell-functions.html), I showed how the combinators can be implemented as native Haskell functions. This allows to evaluate the combinators directly in Haskell without the need for a graph reduction engine.

The parser and the compiler of the first post could be reused without any changes. I just had to plug in a different execution engine. This time based on native Haskell functions instead of graph reduction.

I also did some performance measurements and found that the version unsing native Haskell functions is about 10-100 times faster than the graph reduction engine.

Another significant finding was that the performance of functions with two or more arguments was significantly worse than the performance of functions with one argument. 

This is caused by the inefficient code generation of the classic bracket abstraction: [The output size grows quadratic](https://tromp.github.io/cl/LC.pdf) with internal complexity and number of variables. As each additional combinator or application will require additional execution time it’s easy to see why a quadratic growth in combinator code size will drastically decrease performance. There have been many attempts to optimize bracket abstraction by [introducing additional combinators](https://www.cantab.net/users/antoni.diller/brackets/intro.html) and by applying additional optimization rules.

**In the present post** i will show how to significantly improve the performnce by using an alternative abstraction algorithm. This algorithm is based on the paper [Optimizing bracket abstraction](https://okmij.org/ftp/tagless-final/ski.pdf) by Oleg Kiselyov.

My implementation closely follows Ben Lynn's implementation of Kiselyov's algorithm in [his blog post](https://crypto.stanford.edu/~blynn/lambda/kiselyov.html). I have made only minor changes to make the code more readable and to make it work with the parser and compiler of the first post.

## From λ-calculus to combinators

My parser can parse programs of a very rudimentary language that is basically just pure λ-calculus plus integers. Here is an example:

```haskell
sqr  = λx. * x x
main = sqr 3
```

The parser will produce an environment of top-level definitions from this program:


```haskell
[("sqr",Lam "x" (App (App (Var "*") (Var "x")) (Var "x"))), 
 ("main",App (Var "sqr") (Int 3))]
```

Data types for λ-expressions and the environment are defined as follows:

```haskell
data Expr
  = App Expr Expr
  | Var String
  | Int Integer
  | Lam String Expr
  deriving (Eq, Show)

type Environment = [(String, Expr)]
```

Now we can define a compiler that translates such λ-expressions to combinator terms. 

Our journey begins by translating λ-expressions to a data type `DB` which is quite similar to the λ-calculus terms but uses indices instead of variable names. This is done by the function `deBruijn`:

```haskell
data Peano = Succ Peano | Zero deriving Show
data DB = N Peano | L DB | A DB DB | Free String | IN Integer deriving Show

deBruijn :: Expr -> DB
deBruijn = go [] where
  go binds = \case
    Var x -> maybe (Free x) N $ index x binds
    Lam x t -> L $ go (x:binds) t
    App t u -> A (go binds t) (go binds u)
    Int i -> IN i  

index :: Eq a => a -> [a] -> Maybe Peano
index x xs = lookup x $ zip xs $ iterate Succ Zero    
```

Lets see how this works on our example `sqr` and `main` functions:

```haskell
  let source = [r|
        sqr  = \x. * x x
        main = sqr 3
      |]
  let env = parseEnvironment source
  putStrLn "The parsed environment of named lambda expressions:"
  mapM_ print env
  putStrLn ""
  putStrLn "The expressions in de Bruijn notation:"
  mapM_ (print . Data.Bifunctor.second deBruijn) env
```
This will produce the following output:

```haskell
The parsed environment of named lambda expressions:
("sqr",  Lam "x" (App (App (Var "*") (Var "x")) (Var "x")))
("main", App (Var "sqr") (Int 3))

The expressions in de Bruijn notation:
("sqr",  L (A (A (Free "*") (N Zero)) (N Zero)))
("main", A (Free "sqr") (IN 3))
```

It's easy to see that the de Bruijn notation is just a different representation of the λ-calculus terms. The only difference is that the variable names are replaced by indices.
This is quite helpful as it allows to systematically adress variables by their respective position without having to deal with arbitrary variable names.

But why are we using Peano numbers for the indices? Why not just use integers? 
Well it's definitely possible to [use integers instead of Peano numbers](https://crypto.stanford.edu/~blynn/lambda/cl.html). 
But there is a good reason to use Peano numbers in our case:
In the subsequent compilation steps we want to be able to do pattern matching on the indices. This is not possible with integers but it is possible with Peano numbers, because they are defined as an algebraic data type:
  
```haskell
data Peano = Succ Peano | Zero
``` 

Now we'll take a look at the next step in the compilation process. The function `convert` translates the de Bruijn notation to a data type `CL` which represents the combinator terms. 



```haskell
convert :: ((Int, CL) -> (Int, CL) -> CL) -> [(String, Expr)] -> DB -> (Int, CL)
convert (#) env = \case
  N Zero -> (1, Com I)
  N (Succ e) -> (n + 1, (0, Com K) # t) where t@(n, _) = rec $ N e
  L e -> case rec e of
    (0, d) -> (0, Com K :@ d)
    (n, d) -> (n - 1, d)
  A e1 e2 -> (max n1 n2, t1 # t2) where
    t1@(n1, _) = rec e1
    t2@(n2, _) = rec e2
  IN i -> (0, INT i)
  Free s -> convertVar (#) env s
  where rec = convert (#) env
```

xxx







Combinator terms are defined as follows:

```haskell
data CL = Com Combinator | INT Integer | CL :@ CL

data Combinator = I | K | S | B | C | Y | R | B' | C' | S' | T |
                  ADD | SUB | MUL | DIV | REM | SUB1 | EQL | GEQ | ZEROP  
  deriving (Eq, Show)
```




## to be continued...

## performance comparison

![Alt text](image.png)

## Conclusion
