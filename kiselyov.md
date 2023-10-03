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
(You will find the complete code in [Kiselyov.hs](https://github.com/thma/lambda-ski/blob/main/src/Kiselyov.hs)

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

Lets see how this works on a simple `main` functions:

```haskell
  let source = "main = λx y. * x y"
  let env = parseEnvironment source
  putStrLn "The parsed environment of named lambda expressions:"
  mapM_ print env
  putStrLn ""
  putStrLn "The main expression in de Bruijn notation:"
  mapM_ (print . Data.Bifunctor.second deBruijn) env
```
This will produce the following output:

```haskell
The parsed environment of named lambda expressions:
("main",Lam "x" (Lam "y" (App (App (Var "*") (Var "x")) (Var "y"))))

The main expression in de Bruijn notation:
("main",L (L (A (A (Free "*") (N (Succ Zero))) (N Zero))))
```

It's easy to see that the de Bruijn notation is just a different representation of the λ-term. The only difference is that the variable names are replaced by indices.
The innermost lambda-abstraction binds the variable `y` which is represented by the index `Zero`. The next lambda-abstraction binds the variable `x` which is represented by the index `Succ Zero`. 
This notation is quite helpful as it allows to systematically adress variables by their respective position in a complex term.

But why are we using Peano numbers for the indices? Why not just use integers? 
Well it's definitely possible to [use integers as indices](https://crypto.stanford.edu/~blynn/lambda/cl.html). 
But there is a good reason to use Peano numbers in our case:
In the subsequent compilation steps we want to be able to do pattern matching on the indices. This is possible with Peano numbers, because they are defined as an algebraic data type:
  
```haskell
data Peano = Succ Peano | Zero
``` 

Starting with the de Bruijn notation Ben Lynn's implementation of Kiselyov's algorithm builds up a series of six increasingly optimized compilers that translate λ-expressions to combinator terms:

- a plain compiler without any optimizations (`compilePlain`)
- a compiler that implements K-optimization (`compileK`)
- a compiler that implements K- and Eta-optimization (`compileEta`)
- a compiler that generates code with *Bulk Combinators* (`compileBulk`)
- a compiler that eliminates *Bulk Combinators* with linear size(`compileBulkLinear`)
- a compiler that eliminates *Bulk Combinators* with logarithmic size(`compileBulkLog`)

I'll don't want to go into all the details of the algorithms. [Ben's blog post](https://crypto.stanford.edu/~blynn/lambda/kiselyov.html) is a great resource for this. I'll just give a brief overview of the compilation outputs of the different compilers. And then I'll focus on performance comparisons between the different approaches.
I will use [my original compiler](https://github.com/thma/lambda-ski/blob/main/src/LambdaToSKI.hs) `compileBracket` based on the classic (recursively optimized) bracket abstraction as a baseline for the performance comparisons.

### The simple `main` example

```haskell
main = λx y. * x y
```

| Compiler | Output |
| --- | --- |
| `compileBracket` | `MUL` |
| `compilePlain` | `R I(B S(B(B MUL)(B K I)))` |
| `compileK` | `R I(B B(B MUL I)))` |
| `compileEta` | `MUL` |
| `compileBulk` | `MUL` |
| `compileBulkLinear` | `MUL` |
| `compileBulkLog` | `MUL` |

From this simple example it's obvious that `compilePlain` and `compileK` generate a lot of redundant code. All the other compilers generate the same output as the baseline.

Please also note that the Kiselyov algorithms may emit code for an additional `R` combinator with the following reduction rule:

```haskell
R f g x = g x f  
```

### The factorial function

```haskell
fact = y(λf n. if (is0 n) 1 (* n (f (sub1 n))))
main = fact 100

-- in de Bruijn Notation
("fact", A (Free "y") (L (L (A (A (A (Free "if") (A (Free "is0") (N Zero))) (IN 1)) (A (A (Free "*") (N Zero)) (A (N (Succ Zero)) (A (Free "sub1") (N Zero))))))))
("main", A (Free "fact") (IN 100))
```

| Compiler | Output |
| --- | --- |
| `compileBracket` | `Y(B' S(C' IF ZEROP 1)(B' S MUL(C' S K SUB1))) 100` |
| `compilePlain` | `Y(B(S(R 1(B IF(B ZEROP I))))(B(S(B MUL I))(R(B SUB1 I)(B S(B K I))))) 100` |
| `compileK` | `Y(B(S(C(B IF(B ZEROP I)) 1))(B(S(B MUL I))(R(B SUB1 I)(B B I)))) 100` |
| `compileEta` | `Y(B(S(C(B IF ZEROP) 1))(B(S MUL)(R SUB1 B))) 100` |
| `compileBulk` | `Y(B(S(C(B IF ZEROP) 1))(B(S MUL)(C C SUB1 B))) 100` |
| `compileBulkLinear` | `Y(B(S(C(B IF ZEROP) 1))(B(S MUL)(C C SUB1 B))) 100` |
| `compileBulkLog` | `Y(B(S(C(B IF ZEROP) 1))(B(S MUL)(C C SUB1 B))) 100` |


What's interesting here is that only `compileEta` produces code of the same size as the baseline. All others produce code that uses at least one more combinator. Again `compilePlain` and `compileK` generate the largest code sizes.

### The fibonacci function

```haskell
fib  = y(λf n. if (is0 n) 1 (if (eql n 1) 1 (+ (f (sub1 n)) (f (sub n 2)))))
main = fib 10

-- in de Bruijn notation
("fib", A (Free "y") (L (L (A (A (A (Free "if") (A (Free "is0") (N Zero))) (IN 1)) (A (A (A (Free "if") (A (A (Free "eql") (N Zero)) (IN 1))) (IN 1)) (A (A (Free "+") (A (N (Succ Zero)) (A (Free "sub1") (N Zero)))) (A (N (Succ Zero)) (A (A (Free "sub") (N Zero)) (IN 2)))))))))
("main", A (Free "fib") (IN 10))
```

| Compiler | Output |
| --- | --- |
| `compileBracket` | `Y(B' S(C' IF ZEROP 1)(B' S(C' IF(C EQL 1) 1)(S' S(B' S(K ADD)(C' S K SUB1))(C' S K(C SUB 2))))) 10` |
| `compilePlain` | `Y(B(S(R 1(B IF(B ZEROP I))))(B(S(R 1(B IF(R 1(B EQL I)))))(S(B S(B(B ADD)(R(B SUB1 I)(B S(B K I)))))(R(R 2(B SUB I))(B S(B K I)))))) 10` |
| `compileK` | `Y(B(S(C(B IF(B ZEROP I)) 1))(B(S(C(B IF(C(B EQL I) 1)) 1))(S(B S(B(B ADD)(R(B SUB1 I)(B B I))))(R(C(B SUB I) 2)(B B I))))) 10` |
| `compileEta` | `Y(B(S(C(B IF ZEROP) 1))(B(S(C(B IF(C EQL 1)) 1))(S(B S(B(B ADD)(R SUB1 B)))(R(C SUB 2) B)))) 10` |
| `compileBulk` | `Y(B(S(C(B IF ZEROP) 1))(B(S(C(B IF(C EQL 1)) 1))(S2(B2 ADD(C C SUB1 B))(C C(C SUB 2) B)))) 10` |
| `compileBulkLinear` | `Y(B(S(C(B IF ZEROP) 1))(B(S(C(B IF(C EQL 1)) 1))(B(B S) B S(B B B ADD(C C SUB1 B))(C C(C SUB 2) B)))) 10` |
| `compileBulkLog` | `Y(B(S(C(B IF ZEROP) 1))(B(S(C(B IF(C EQL 1)) 1))(S B I(B(B S) B) I(S B I B ADD(C C SUB1 B))(C C(C SUB 2) B)))) 10` |


Here we see that `compileEta` produce code of the same size as the baseline. `compileBulk` generates code with one less combinator.

Please also note that `compileBulk` now emits code for additional bulk combinators `S2` and `B2`. I'll come back to the semantics of these later.


### The ackermann function

```haskell
ack  = y(λf n m. if (is0 n) (+ m 1) (if (is0 m) (f (sub1 n) 1) (f (sub1 n) (f n (sub1 m)))))
main = ack 2 2

-- in de Bruijn notation
("ack", A (Free "y") (L (L (L (A (A (A (Free "if") (A (Free "is0") (N (Succ Zero)))) (A (A (Free "+") (N Zero)) (IN 1))) (A (A (A (Free "if") (A (Free "is0") (N Zero))) (A (A (N (Succ (Succ Zero))) (A (Free "sub1") (N (Succ Zero)))) (IN 1))) (A (A (N (Succ (Succ Zero))) (A (Free "sub1") (N (Succ Zero)))) (A (A (N (Succ (Succ Zero))) (N (Succ Zero))) (A (Free "sub1") (N Zero))))))))))
("main", A (A (Free "ack") (IN 2)) (IN 2))
```

| Compiler | Output |
| --- | --- |
| `compileBracket` | `Y(B' S(B S(C'(B S K)(B IF ZEROP)(C ADD 1)))(S'(B S(S(K S)))(B' S(K(S(B IF ZEROP)))(B' S(K K)(C' S(C' S K SUB1)(K 1))))(S'(B S(S(K(B S K))))(C' S K SUB1)(C' S(S(K(B S K)))(K SUB1))))) 2 2` |
| `compilePlain` | `Y(B(S(B S(R(R 1(B ADD I))(B S(B(B IF)(B(B ZEROP)(B K I)))))))(S(B S(B(B S)(B(B(S(B IF(B ZEROP I))))(B(B(R 1))(R(B(B SUB1)(B K I))(B S(B(B S)(B(B K)(B K I)))))))))(S(B S(B(B S)(R(B(B SUB1)(B K I))(B S(B(B S)(B(B K)(B K I)))))))(B(R(B SUB1 I))(B(B S)(R(B K I)(B S(B(B S)(B(B K)(B K I)))))))))) 2 2` |
| `compileK` | `Y(B(S(B S(R(C(B ADD I) 1)(B B(B IF(B ZEROP I))))))(S(B S(B(B S)(B(B(C(B IF(B ZEROP I))))(B(R 1)(R(B SUB1 I)(B B I))))))(S(B S(B(B B)(R(B SUB1 I)(B B I))))(B(R(B SUB1 I))(B(B B)(R I(B B I))))))) 2 2` |
| `compileEta` | `Y(B(S(B S(R(C ADD 1)(B B(B IF ZEROP)))))(S(B S(B(B S)(B(B(C(B IF ZEROP)))(B(R 1)(R SUB1 B)))))(S(B S(B(B B)(R SUB1 B)))(B(R SUB1)(B B))))) 2 2` |
| `compileBulk` | `Y(B(S2(C C(C ADD 1)(B B(B IF ZEROP))))(S3(B2(C(B IF ZEROP))(C C2 1(C C SUB1 B)))(S2(B2 B(C C SUB1 B))(C C2 SUB1(B B))))) 2 2` |
| `compileBulkLinear` | `Y(B(B(B S) B S(C C(C ADD 1)(B B(B IF ZEROP))))(B(B S) B(B(B S) B S)(B B B(C(B IF ZEROP))(C(B(B C) B C) 1(C C SUB1 B)))(B(B S) B S(B B B B(C C SUB1 B))(C(B(B C) B C) SUB1(B B))))) 2 2` |
| `compileBulkLog` | `Y(B(S B I(B(B S) B) I(C C(C ADD 1)(B B(B IF ZEROP))))(B(B(B(B S) B))(S B I)(B(B S) B) I(S B I B(C(B IF ZEROP))(C(S B I(B(B C) B) I) 1(C C SUB1 B)))(S B I(B(B S) B) I(S B I B B(C C SUB1 B))(C(S B I(B(B C) B) I) SUB1(B B))))) 2 2` |

As mentioned in my last post the output size of braxcket abstraction grows quadratic with the number of variables.
In this case with three variables the output size for the bracket abstraction is already significantly larger than for 
the previous example with two variables.

Now the Kiselyov algorithms really start to shine. `compileEta` produces code is significantly smaller as the baseline. And `compileBulk` output is even smaller.



### The tak function

```haskell
tak  = y(λf x y z. (if (geq y x) z (f (f (sub1 x) y z) (f (sub1 y) z x) (f (sub1 z) x y ))))
main = tak 7 4 2

-- in de Bruijn notation
("tak",A (Free "y") (L (L (L (L (A (A (A (Free "if") (A (A (Free "geq") (N (Succ Zero))) (N (Succ (Succ Zero))))) (N Zero)) (A (A (A (N (Succ (Succ (Succ Zero)))) (A (A (A (N (Succ (Succ (Succ Zero)))) (A (Free "sub1") (N (Succ (Succ Zero))))) (N (Succ Zero))) (N Zero))) (A (A (A (N (Succ (Succ (Succ Zero)))) (A (Free "sub1") (N (Succ Zero)))) (N Zero)) (N (Succ (Succ Zero))))) (A (A (A (N (Succ (Succ (Succ Zero)))) (A (Free "sub1") (N Zero))) (N (Succ (Succ Zero)))) (N (Succ Zero))))))))))
("main",A (A (A (Free "tak") (IN 7)) (IN 4)) (IN 2))
```

| Compiler | Output |
| --- | --- |
| `compileBracket` | `Y(B' S(B'(S(K S))(S(K S))(B' S(K IF)(B' S GEQ K)))(S'(B S(S(K(B S(S(K S))))))(S'(B S(S(K(B S(S(K S))))))(S'(B'(S(K(B'(S(K S)) K S))) K S) K(C' S K SUB1))(C'(B'(S(K(B S K))) S(S(K S)))(C' S K SUB1)(B K K)))(C'(B S(S(K(B'(S(K S)) K S))))(C'(B'(S(K S)) K S)(C' S K SUB1) K)(K K)))) 7 4 2` |
| `compilePlain` | `Y(B(S(B S(B(B S)(B(R I)(B(B S)(B(B(B IF))(B(S(B S(B(B GEQ)(B K I))))(B(B K)(B K I)))))))))(S(B S(B(B S)(B(B(B S))(S(B S(B(B S)(B(B(B S))(S(B S(B(B S)(B(B(B S))(B(B(B K))(B(B K)(B K I))))))(B(B(R I))(B(B(B S))(B(R(B K I))(B(B S)(B(B(B S))(R(B(B(B SUB1))(B(B K)(B K I)))(B S(B(B S)(B(B(B S))(B(B(B K))(B(B K)(B K I))))))))))))))))(R(B(B K)(B K I))(B S(B(B S)(B(B(B S))(B(B(R I))(B(B(B S))(B(R(B(B SUB1)(B K I)))(B(B S)(B(B(B S))(B(B(B K))(B(B K)(B K I))))))))))))))))(B(R(B K I))(B(B S)(B(B(B S))(R(B(B K)(B K I))(B S(B(B S)(B(B(B S))(B(B(R(B SUB1 I)))(B(B(B S))(B(B(B K))(B(B K)(B K I)))))))))))))) 7 4 2` |
| `compileK` | `Y(B(S(B S(B(B S)(B(R I)(B(B B)(B(B IF)(B(C(B GEQ I)) I)))))))(S(B S(B(B S)(B(B(B S))(S(B S(B(B S)(B(B(B S))(S(B B(B B(B B I)))(B(B(R I))(B(B(B B))(B(R I)(B(B B)(R(B SUB1 I)(B B I))))))))))(R I(B B(B C(B(B C)(B(R I)(B(B B)(R(B SUB1 I)(B B I))))))))))))(B(R I)(B(B B)(B(B C)(R I(B B(B C(R(B SUB1 I)(B B I)))))))))) 7 4 2` |
| `compileEta` | `Y(B(S(B S(B(B S)(B(B IF)(C GEQ)))))(S(B S(B(B S)(B(B(B S))(S(B S(B(B S)(B(B(B S))(S(B B(B B B))(R SUB1 B)))))(B C(B(B C)(R SUB1 B)))))))(B(B C)(B C(R SUB1 B))))) 7 4 2` |
| `compileBulk` | `Y(B(S3(B2 IF(C GEQ)))(S4(S4(S B3(C C SUB1 B))(B C2(C C SUB1 B)))(B2 C(B C(C C SUB1 B))))) 7 4 2` |
| `compileBulkLinear` | `Y(B(B(B S) B(B(B S) B S)(B B B IF(C GEQ)))(B(B S) B(B(B S) B(B(B S) B S))(B(B S) B(B(B S) B(B(B S) B S))(S(B B(B B B))(C C SUB1 B))(B(B(B C) B C)(C C SUB1 B)))(B B B C(B C(C C SUB1 B))))) 7 4 2` |
| `compileBulkLog` | `Y(B(B(B(B(B S) B))(S B I)(B(B S) B) I(S B I B IF(C GEQ)))(S B I(S B I(B(B S) B)) I(S B I(S B I(B(B S) B)) I(S(B(B B)(S B I) B)(C C SUB1 B))(B(S B I(B(B C) B) I)(C C SUB1 B)))(S B I B C(B C(C C SUB1 B))))) 7 4 2` |

In this example with four variables the trend continues. `compileEta` produces code is significantly smaller as the baseline. And `compileBulk` output now is only about 1/3 of the baseline.

## Executing Bulk Combinators

We have seen that Kisekyov's algorithms produce code that makes use of *Bulk Combinators* like `S4`, `B3` or `C2`. Ben Lynn defines the semantics of these combinators as follows:

<img style="align:center;" src="https://latex.codecogs.com/svg.image?\begin{align*}B_{n&plus;1}&=B'B_n\\C_{n&plus;1}&=C'C_n\\S_{n&plus;1}&=S'S_n\end{align*}" />

where `B'`, `C'` and `S'` defined as follows:

<img style="align:center;" src="https://latex.codecogs.com/svg.image?\begin{align*}B'&=BB\\C'&=B(BC)B\\S'&=B(BS)B\end{align*}" />

Ben also defines the following function that converts a combinator term with *Bulk Combinators* to a combinator term with only standard combinators:

```haskell
breakBulkLinear :: Combinator -> Int -> CL
breakBulkLinear B n = iterate (comB' :@) (Com B) !! (n - 1)
breakBulkLinear C n = iterate (comC' :@) (Com C) !! (n - 1)
breakBulkLinear S n = iterate (comS' :@) (Com S) !! (n - 1)

comB' :: CL
comB' = Com B:@ Com B
comC' :: CL
comC' = Com B :@ (Com B :@ Com C) :@ Com B
comS' :: CL
comS' = Com B :@ (Com B :@ Com S) :@ Com B
```

As we have seen in the output of the `compileBulkLinear` this conversion expands the code size. To avoid this expansion of the combinator code I have implemented a solution to directly execute *Bulk Combinators*.

At the moment I have only implemented it in the [Haskell-In-Haskell](https://wiki.haskell.org/wikiupload/0/0a/TMR-Issue10.pdf) inspired HHI-Reducer. Implementing it for the Graph Reduction Engine is left as an exercise for the reader ;-).

```haskell
-- | apply a CExpr of shape (CFun f) to argument x by evaluating (f x)
infixl 0 !
(!) :: CExpr -> CExpr -> CExpr
(CFun f) ! x = f x

-- | "link" a compiled expression into Haskell native functions.
--   application terms will be transformed into real (!) applications
--   combinator symbols will be replaced by their actual function definition
link :: CombinatorDefinitions -> CExpr -> CExpr
link definitions (CApp fun arg) = link definitions fun ! link definitions arg
link definitions (CComb comb)   = case lookup comb definitions of
                                    Nothing -> resolveBulk comb
                                    Just e  -> e
link _definitions expr          = expr


resolveBulk :: Combinator -> CExpr
resolveBulk (BulkCom "B" n) = iterate (comB' !) comB !! (n-1)
resolveBulk (BulkCom "C" n) = iterate (comC' !) comC !! (n-1)
resolveBulk (BulkCom "S" n) = iterate (comS' !) comS !! (n-1)

comS :: CExpr
comS = CFun (\f -> CFun $ \g -> CFun $ \x -> f!x!(g!x))                    -- S F G X = F X (G X)

comS' :: CExpr
comS' = CFun (\p -> CFun $ \q -> CFun $ \r -> CFun $ \s -> p!(q!s)!(r!s))  -- S' P Q R S = P (Q S) (R S)
```


## performance comparison

So far we have seen that for functions with more than two variables the Kiselyov algorithms generate code that is significantly smaller than optimized versions of classic bracket abstraction. 
But what about performance? Is the code generated by the Kiselyov algorithms also faster?

To answer this question I have set up a benchmarking suite based on the [micro-benchmarking framework Criterion](http://www.serpentine.com/criterion/). 

In my suite I am testing the performance of combinations of the following components:

- the compilers `compileBracket`, `compileEta` and `compileBulk` from the previous section
- the function factorial, fibonacci, ackermann and tak from the previous section
- the execution backenda Graph Reduction Engine and the native Haskell functions implementaion from my previous post. I have not implemented the Bulk combinators in the graph reduction engine. So I am only testing this backend only with the `compileBracket` and `compileEta` compilers.

So lets start with an overview of the results for the Graph Reduction Backend.



## 


![Alt text](image.png)

## Conclusion
