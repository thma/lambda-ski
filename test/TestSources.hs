{-# LANGUAGE QuasiQuotes #-}
module TestSources where

import Text.RawString.QQ  

type SourceCode = String

ackermann :: SourceCode 
ackermann = [r|
  expected = 7
  ack  = y(λf n m -> if (is0 n) (+ m 1) (if (is0 m) (f (sub1 n) 1) (f (sub1 n) (f n (sub1 m)))))
  main = ack 2 2
|]

factorial :: SourceCode
factorial = [r| 
  expected = 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
  fact = y(λf n -> if (is0 n) 1 (* n (f (sub1 n))))
  main = fact 100
|]

fibonacci :: SourceCode
fibonacci = [r| 
  expected = 89
  fib  = y(λf n -> if (is0 n) 1 (if (eql n 1) 1 (+ (f (sub1 n)) (f (sub n 2)))))
  main = fib 10
|]

gaussian :: SourceCode
gaussian = [r| 
  expected = 5050
  gaussianSum  = y(λf n -> (if (is0 n) 0 (+ n (f (sub1 n)))))
  main = gaussianSum 100
|]

tak :: SourceCode
tak = [r| 
  expected = 4
  tak  = y(λf x y z -> (if (geq y x) z (f (f (sub1 x) y z) (f (sub1 y) z x) (f (sub1 z) x y ))))
  main = tak 7 4 2 --18 6 3
|]