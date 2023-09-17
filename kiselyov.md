# Optimizing bracket abstraction for Combinator based interpreters

## Abstract


## Introduction

In previous blog posts i have shown how functional languages can be implemented using a small set of combinators. The first post, [Implementing a functional language with Graph Reduction](https://thma.github.io/posts/2021-12-27-Implementing-a-functional-language-with-Graph-Reduction.html) proceeded in three major steps to implement a functional language:

- A parser for a tiny functional language based on the untyped λ-calculus.

- A compiler from λ-calculus to a fixed set of combinatory logic combinators (S,K,I,B,C and Y (aka. SICKBY)).

- A graph-reduction engine which implements the combinator rewrite rules as an efficient graph reduction

The second post, [Evaluating SKI combinators as native Haskell functions](https://thma.github.io/posts/2022-02-05-Evaluating-SKI-combinators-as-native-Haskell-functions.html), demonstrated how 



In this post i will show how to optimize the implementation of the lambda calculus using combinators. The optimization is based on the paper [Optimizing bracket abstraction](http://okmij.org/ftp/Computation/lambda-calc.html#bracket-opt) by Oleg Kiselyov.