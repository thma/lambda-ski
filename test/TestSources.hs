module TestSources where


ackermann :: String 
ackermann = "expected = 7 \n" ++
  "ack  = y(λf n m -> if (is0 n) (+ m 1) (if (is0 m) (f (sub1 n) 1) (f (sub1 n) (f n (sub1 m)))))\n" ++
  "main = ack 2 2"

factorial :: String
factorial = "expected = 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000\n" ++
  "fact = y(λf n -> if (is0 n) 1 (* n (f (sub1 n))))\n" ++
  "main = fact 100"

fibonacci :: String
fibonacci = "expected = 89\n" ++
  "fib  = y(λf n -> if (is0 n) 1 (if (eql n 1) 1 (+ (f (sub1 n)) (f (sub n 2)))))\n" ++
  "main = fib 10" 

gaussian :: String
gaussian = "expected = 5050\n" ++
  "gaussianSum  = y(λf n -> (if (is0 n) 0 (+ n (f (sub1 n)))))\n" ++
  "main = gaussianSum 100" 

tak :: String
tak = "expected = 4\n" ++
  "tak  = y(λf x y z -> (if (geq y x) z (f (f (sub1 x) y z) (f (sub1 y) z x) (f (sub1 z) x y ))))\n" ++
  "main = tak 7 4 2"