# lambda-ski
Implementing a small functional language with a combinator based graph-reduction machine

I took a parser for tiny functional language from [https://crypto.stanford.edu/~blynn/lambda/sk.html](https://crypto.stanford.edu/~blynn/lambda/sk.html) which allows 
to write programs like the following:

```haskell
Y    = λf -> (λx -> x x)(λx -> f(x x))
fact = Y(λf n -> if (eq 0 n) 1 (* n (f (sub n 1))))
main = fact 10
```

I'm then applying classic compilation to S,K,I,B,C combinators by λ-abstraction and some optimization rules.

These combinator terms are then allocated into a graph data-structure.

This graph is then reduced by applying combinator graph-reduction.

this part is based on a SML implementation i did some 30 years ago:
[https://github.com/thma/mg-book-sml-sources/blob/main/sml-sources/com41.sml](https://github.com/thma/mg-book-sml-sources/blob/main/sml-sources/com41.sml)

this is heavliy
