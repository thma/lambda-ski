module Example(fac, main) where

fac :: Int -> Int
fac 0 = 1
fac n = n * fac(n - 1)

main :: IO ()
main = do
  putStrLn "computing some factorials"
  print $ map fac [0..10]