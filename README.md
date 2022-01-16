# lambda-ski

[![Actions Status](https://github.com/thma/lambda-ski/workflows/Haskell%20CI/badge.svg)](https://github.com/thma/lambda-ski/actions)

## Abstract

Implementing a small functional language with a classic combinator based graph-reduction machine in Haskell.

The implementation is structured into three parts:

1. A λ-calculus parser from [A Combinatory Compiler](https://crypto.stanford.edu/~blynn/lambda/sk.html) 
which was extended to cover a tiny functional language based on the untyped λ-calculus.

2. A compiler from λ-calculus to combinatory logic combinators (S,K,I,B,C and Y) which is based on bracket-abstraction and some optimization rules.

3. A graph-reducer. Combinator terms are allocated into a graph data-structure.
Which  is then reduced by applying combinator graph-reduction. The destructive inplace reduction of the graph is made possible by using `STRef` mutable references. 

## Introduction

In my [last blog post](https://thma.github.io/posts/2021-04-04-Lambda-Calculus-Combinatory-Logic-and-Cartesian-Closed-Categories.html) I presented two ways of transforming λ-terms into variable free representations: 
- bracket abstraction to combinatory logic terms (SKI) and 
- bracket abstraction to terms of closed cartesian categories (CCC).

I demonstrated that both representations are equivalent as they imply the same reduction rules. 

My original intention was to extend an existing Haskell CCC implementation to a proof-of-concept implementation of a small functional language. I even promised to cover this in my next blog post.

I invested a lot of time in this idea but I failed to get it off the ground. [At least the code of these experiments has been preserved](https://github.com/thma/lambda-cat).

So I came back to writing a SKI graph-reduction as the backend of my language implementation. This is a well-worn path. I took the basic ideas from the classic [compiling functional languages](https://www.goodreads.com/book/show/3468677-compiling-functional-languages) which dates back to 1988.

Fortunately, I did not fail this time! 
In the following I'm explaining my implementation approach. I'll also share some of my insights and talk about possible future extensions.

## representing λ-expressions

I'm aiming at a very rudimentary language that is basically just pure λ-calculus plus integers. Here is an example:

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

Language implemetors have thus experimented with many ways to tackle this issue. One of the most influential ideas was to completely get rid of variables by abstracting them. 

The earliest version of this approach was [the SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus) invented by Haskell Curry and Moses Schönfinkel.

A λ-term that does not contain any free variables is said to be closed. Closed lambda terms are also called *combinators*. 

Schönfinkel and Curry found out that any closed λ-term can be rewritten in terms of three basic combinators I, K and S (in fact only *K* and *S* are essential, as *I* can be expressed as SKK):

<img style="align:center;" src="https://latex.codecogs.com/gif.latex?\begin{array}{rcl}&space;I&space;&&space;=&space;&&space;\lambda&space;x.x&space;\\&space;K&space;&&space;=&space;&&space;\lambda&space;x.&space;\lambda&space;y.x&space;\\&space;S&space;&&space;=&space;&&space;\lambda&space;f.(\lambda&space;g.(\lambda&space;x.fx(gx)))&space;\end{array}" title="\begin{array}{rcl} I & = & \lambda x.x \\ K & = & \lambda x. \lambda y.x \\ S & = & \lambda f.(\lambda g.(\lambda x.fx(gx))) \end{array}" />


In Haskell these combinators can simply be defined as:

```haskell
i x = x
k x y = x
s f g x = f x (g x)
```



## The basic abstraction rules

The idea of bracket abstraction is to rewrite any closed λ-term in terms of I, K and S.
This recursive transformation is defined by the following equations:

<img src="https://latex.codecogs.com/gif.latex?\begin{array}{rcl}&space;\left&space;\lceil&space;\lambda&space;x.x&space;\right&space;\rceil&space;&&space;=&space;&&space;I&space;\\&space;\left&space;\lceil&space;\lambda&space;x.y&space;\right&space;\rceil&space;&&space;=&space;&&space;K&space;y&space;\\&space;\left&space;\lceil&space;\lambda&space;x.M&space;N&space;\right&space;\rceil&space;&&space;=&space;&&space;S&space;\left&space;\lceil&space;\lambda&space;x.M&space;\right&space;\rceil&space;\left&space;\lceil&space;\lambda&space;x.N&space;\right&space;\rceil&space;\end{array}" title="\begin{array}{rcl} \left \lceil \lambda x.x \right \rceil & = & I \\ \left \lceil \lambda x.y \right \rceil & = & K y \\ \left \lceil \lambda x.M N \right \rceil & = & S \left \lceil \lambda x.M \right \rceil \left \lceil \lambda x.N \right \rceil \end{array}" />

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

The [sourcecode for this section can be found here](https://github.com/thma/lambda-ski/blob/main/src/LambdaToSKI.hs)

## Graph-reduction in a nutshell

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
  Please note that in this state the `S` is our *redex* (i.e. the left-most ancestor of the root node) and *saturated* (i.e all three arguments of the combinator) are populated, so according to the reduction rule `s f g x = f x (g x)` we expect to see a reduction `S MUL I (ADD 3 2) = MUL (ADD 3 2) (I (ADD 3 2))` in step 1.

- **Step 1**: As expected the first reduction step mutates the graph to represent `MUL (ADD 3 2) (I (ADD 3 2))`. Please note that both occurrences of `(ADD 3 2)` are represented by references to one and the same node. 

- **Step 2**: Now `MUL` has become the *redex* (short for reducible expression). But this time both arguments `(ADD 3 2)` and `I (ADD 3 2)` are not in normal-form and thus have to be reduced first before `MUL` can be executed. So first `(ADD 3 2)` is reduced to `5`. Please note that both references to the former `(ADD 3 2)` node now point to `5`. So in effect the `I (ADD 3 2)` node has changed to `I 5` as  `(ADD 3 2)` was a shared node.

- **Step 3**: next the `I 5` node is reduced according to the equation `i x = x`. That is, the reference to the application node `I @ 5` is modified to directly point to `5` instead. Please note that both arguments point to one and the same numeric value `5`.

- **Step 4**: As a result of the transformation in step 3 both arguments of `MUL` are in normal-form. So now `MUL 5 5` can be performed: Accordingly the root node is now changed to `25`.

Now that we have a basic understanding of the ideas behind graph-reduction we will have a closer look at the actual implementation in the following sections.

## Allocating a Graph with mutable references

As we have seen in the last section we will have to deal with mutable references in order to implement things like node sharing and in-place mutation of nodes.

I will use the Haskell datatype [`Data.STRef`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-STRef.html) which provides mutable references in the `ST` monad.

Here comes a basic example that demonstrates the basic functionality of `STRef`. A list of numbers is summed up by adding each of them to an accumulator. The accumulator is implemented by a reference `acc` pointing to an initial value of `0`. 
Then we iterate over the list of numbers and update the value of the accumulator by adding each number `x` to it.
Finally the result is read out from the accumulator and extracted from the ST Monad by runST. From this example we can see that `STRef`s work much like pointers in imperative languages:


```haskell
import Data.STRef       (STRef, modifySTRef, newSTRef, readSTRef writeSTRef)
import Control.Monad.ST (runST)

-- | sum up a list of numerical values 
sumST :: Num a => [a] -> a
sumST numbers = runST $ do -- runST takes stateful ST code and makes it pure.
  acc <- newSTRef 0        -- Create an STRef (a mutable variable) to an accumulator
  forM_ numbers $ \x ->    -- iterate over all numbers
    modifySTRef acc (+ x)  -- add each number to what we have in acc.
  readSTRef acc            -- read the value of acc, which will be returned by the runST above.
```

This looks promising. So now lets implement a binary graph for our compiled combinator terms with it: 


```haskell
infixl 5 :@:

data Graph s
  = (STRef s (Graph s)) :@: (STRef s (Graph s))
  | Comb Combinator
  | Num Integer
  deriving (Eq)

data Combinator = I | K | S | B | C | Y | P | ADD | SUB | MUL | DIV | REM | SUB1 | EQL | ZEROP | IF
  deriving (Eq, Show)

```

So we basically mimic the `Expr` data type used to encode λ-expression but without variables and lambda-abstractions. The data type `Combinator` contains constructors for combinators that we intend to implement in the graph-reduction engine.

Next we define a function `allocate` that allows to allocate a 'lambda-abstracted' λ-expression (of type `Expr`) into a reference to a `Graph`:

```haskell
-- | allocate a 'lambda-abstracted' Expr into a referenced Graph
allocate :: Expr -> ST s (STRef s (Graph s))
allocate (Var name) = newSTRef $ Comb $ fromString name
allocate (Int val)  = newSTRef $ Num val
allocate (l :@ r)   = do
  lg <- allocate l
  rg <- allocate r
  newSTRef $ lg :@: rg
allocate (Lam _ _)  = error "lambdas must already be abstracted away!"

-- | lookup Combinator constructors by their names
fromString :: String -> Combinator
fromString "i"    = I
fromString "k"    = K
fromString "s"    = S
fromString "b"    = B
fromString "c"    = C
fromString "y"    = Y
fromString "p"    = P
fromString "+"    = ADD
fromString "sub"  = SUB
fromString "div"  = DIV
fromString "rem"  = REM
fromString "*"    = MUL
fromString "sub1" = SUB1
fromString "eq"   = EQL
fromString "is0"  = ZEROP
fromString "if"   = IF
fromString _c     = error $ "unknown combinator " ++ _c
```

So let's see this in action:

```haskell
ghci> optExpr = ((Var "s" :@ Var "*") :@ Var "i") :@ ((Var "+" :@ Int 3) :@ Int 2)
ghci> graph = allocate optExpr
ghci> runST $ mToString graph 
"(((S :@: MUL) :@: I) :@: ((ADD :@: 3) :@: 2))"
```
 
 I'm using the `mToString` helper function to render `ST s (STRef s (Graph s))` instances:

 ```haskell
mToString :: ST s (STRef s (Graph s)) -> ST s String
mToString g = toString =<< g     

toString :: STRef s (Graph s) -> ST s String
toString graph = do
  g <- readSTRef graph
  toString' g where
    toString' (Comb c) = return $ show c
    toString' (Num i) = return $ show i
    toString' (lP :@: rP) = do
      lG <- readSTRef lP
      rG <- readSTRef rP
      lStr <- toString' lG
      rStr <- toString' rG
      return $ "(" ++ lStr ++ " :@: " ++ rStr ++ ")" 
 ```

Now that we have allocated our expression as an `ST s (STRef s (Graph s))` the next step will be to perform graph reduction on it.

## Performing graph-reduction

First we have to compute the stack of left ancestors - or *spine* - of a graph for an efficient reduction.

In the following diagram I have marked the members of this stack with `->` arrows:

```haskell 
->           @                   
            / \              
           /   \               
          /     @          
         /     / \          
        /     @   2         
->     @     / \     
      / \  ADD  3    
->   @   I            
    / \             
-> S  MUL   
```

The following function `spine` computes this left ancestors' stack by traversing all application nodes to the left:

```haskell
-- we simply represent the stack as a list of references to graph nodes
type LeftAncestorsStack s = [STRef s (Graph s)]

spine :: STRef s (Graph s) -> ST s (LeftAncestorsStack s)
spine graph = spine' graph [] 
  where
    spine' :: STRef s (Graph s) -> LeftAncestorsStack s -> ST s (LeftAncestorsStack s)
    spine' graph stack = do
      g <- readSTRef graph
      case g of
        (l :@: _r) -> spine' l (graph : stack)
        _          -> return (graph : stack)
```

Using this `spine` function we can implement a function `step` that performs a single reduction step on a `Graph` node:

```haskell
step :: STRef s (Graph s) -> ST s ()
step graph = do
  (top:stack) <- spine graph
  node <- readSTRef top
  case node of
    (Comb k) -> reduce k stack
    _        -> return ()
```

If a combinator is found in redex position, `reduce` is called to perform the actual reduction work according to the combinator specific reduction rules.

Let's study this for some of the combinators, starting with the most simple one, `I x = x`:

```haskell
        |
p  ->   @   
       / \
   -> I   x
```

```haskell
reduce :: Combinator -> LeftAncestorsStack s -> ST s ()
reduce I (p : _) = do
  (_I :@: xP) <- readSTRef p
  xVal <- readSTRef xP
  writeSTRef p xVal
```

In this case a reference `p` to `(I :@: xP )` is on top of the stack. The actual value of x is read from `xP` with `readSTRef` and than `p` is made to point to this value by using `writeSTRef`.

The reduction of `S f g x = f x (g x)` is already a bit more involved:

```haskell
            |
p3 ->       @
           / \
p2 ->     @   x
         / \
p1 ->   @   g
       / \
   -> S   f
```

```haskell
reduce S (p1 : p2 : p3 : _) = do
  (_S :@: fP) <- readSTRef p1
  (_  :@: gP) <- readSTRef p2
  (_  :@: xP) <- readSTRef p3
  node1 <- newSTRef $ fP :@: xP
  node2 <- newSTRef $ gP :@: xP
  writeSTRef p3 (node1 :@: node2)
```

In this case reference to f (`fP`), g (`gP`) and x (`xP`) are obtained. Then a new application node is created that represents `((f @ x) @ (g @ x))`. Then `p3` is made to point to this new node.

Binary arithmentic combinators like `ADD` and `MUL` are implemented as follows:

```haskell
reduce ADD (p1 : p2 : _) = binaryMathOp (+) p1 p2
reduce MUL (p1 : p2 : _) = binaryMathOp (*) p1 p2

binaryMathOp ::
  (Integer -> Integer -> Integer) -> -- ^ a binary arithmetic function on Integers like (+)
  STRef s (Graph s) ->               -- ^ first node on the spine stack
  STRef s (Graph s) ->               -- ^ second node on spine stack
  ST s ()                            
binaryMathOp op p1 p2 = do
  (_ :@: xP) <- readSTRef p1
  (_ :@: yP) <- readSTRef p2
  (Num xVal) <- (readSTRef <=< normalForm) xP  -- reduce xP to normal form and obtain its value as xVal
  (Num yVal) <- (readSTRef <=< normalForm) yP  -- reduce yP to normal form and obtain its value as yVal
  writeSTRef p2 (Num $ xVal `op` yVal)         -- apply op on xVal and yVal, modify p2 to point to the resulting value
```

The interesting bit here is that the arithmetic combinators are *strict*, that is they require their arguments to be in normalform. (Please note that `S`, `I`,  `K`, etc. don't have this requirement. They are *non-strict* or *lazy*).

`normalForm` just applies `step` in a loop while the graph has not been reduced to a combinator or an integer:


```haskell
normalForm :: STRef s (Graph s) -> ST s (STRef s (Graph s))
normalForm graph = do
  step graph
  g <- readSTRef graph
  case g of
    _lP :@: _rP -> normalForm graph
    Comb _com   -> return graph
    Num _n      -> return graph
```

Using a helper function `reduceGraph` that computes the normal-form of a graph while staying entirely in the `ST`-Monad, we can finally reduce our tiny toy graph:

```haskell
reduceGraph :: ST s (STRef s (Graph s)) -> ST s (STRef s (Graph s))
reduceGraph graph = do
  gP <- graph
  normalForm gP


ghci> runST $ mToString graph
"(((S :@: MUL) :@: I) :@: ((ADD :@: 3) :@: 2))"
ghci> runST $ mToString $ reduceGraph graph
"25"
```

## Recursion

λ-calculus does not directly support recursion using self-referential functions [(see this nice exposition)](https://sookocheff.com/post/fp/recursive-lambda-functions/). That's why we need a fixed-point combinator to realize recursive operation. Here once again the definition of the factorial function that makes use of the `Y`-Combinator to implement recursive behaviour: 

```haskell
Y    = λf . (λx . x x)(λx . f(x x))
fact = Y(\f n -> if (is0 n) 1 (* n (f (sub1 n))))
main = fact 10
```

With only a few lines of equational reasoning we can demonstrate the special property of the `Y`-combinator when applied to any function `g`:

```haskell
Y g = (λf.(λx.x x)(λx.f(x x))) g  -- (1) by definition of Y
    = (λx.g (x x))(λx.g (x x))    -- (2) by function application of λf
    = g((λx.g (x x))(λx.g (x x))) -- (3) by function application of λx.g(x x) to λx.g(x x)
    = g(Y g)                      -- (4) by equation (2)
```

Applying equation `(4)` repeatedly will lead to:

```haskell
Y g = g(g(Y g))                    -- (5) by equation (4)
    = g(...g(Y g) ...)             -- (6) by repeatedly applying (4)
```

In this way the `Y`-combinator achieves recursion by reproducing a (self-reproducing) copy of the function's self-application with each application of `(4)`.

This self-reproducing pattern becomes even more visible when looking at the graph-structure of  the reduction of `(Y g)`: 

```haskell
                                            __
  @    ==>    @     ==>   @    ==>  ...   @   \
 / \         / \         / \             / \__/
Y   g       g   @       g   @           g
               / \         / \
              Y   g       g   @
                             / \
                            Y   g
```

One can see how at each application of `(4)` another copy of (Y g) is generated and incorporated into the graph as an argument of g.

The last step of the diagram shows that - in the graph - self-reproduction can be achieved by simply bending the argument pointer back to the application node.


This realization leads us to the following implementation of the Y-combinator:

```haskell
reduce Y (p1 : _) = do
  (_YP :@: gP) <- readSTRef p1
  writeSTRef p1 (gP :@: p1)
```

Using this implementation of the Y-combinator instead of the source level defined version `Y = λf.(λx.x x)(λx.f(x x))` reduces the execution time for `fact 10000` by a factor of about 250.

The [sourcecode for this section can be found here](https://github.com/thma/lambda-ski/blob/main/src/LambdaToSKI.hs).


## Next steps

Here are some ideas for possible future extensions and improvements.

- Extending this very basic setup to a fully working pogramming environment with a REPL
- Implement direct and mutual recursion (i.e. `letrec`) for global function definitions
- experimemnt with different bracket abstraction algorithms to improve object code size and execution time.
- Implement bracket abstraction from λ-expressions to [closed cartesian categories](https://thma.github.io/posts/2021-04-04-Lambda-Calculus-Combinatory-Logic-and-Cartesian-Closed-Categories.html) and extend the graph-reduction to also cover the resulting combinators `apply` and `(△)`.
- extend the language to include lists, maybe even provide it with a LISPKIT frontend.
- Add support for implicit and explicit parallelism of the graph-reduction engine.
  (implicit parallelism for strict operations, and an explicit `P`-combinator)

## Related ideas

https://wiki.haskell.org/wikiupload/0/0a/TMR-Issue10.pdf

https://smunix.github.io/kseo.github.io/posts/2016-12-30-write-you-an-interpreter.html

I've implemented this idea in Reducer.hs. But honestly I did not yet fully understand how it works.


