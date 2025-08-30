
{-# LANGUAGE QuasiQuotes #-}
module Main (Main.main) where

import           Control.Monad.ST
import Control.Category ( (>>>) )
import           Data.List        (lookup)
import           Data.Maybe
import           Data.STRef
import           GraphReduction
import           LambdaToSKI
import           CLTerm
import           Parser           (Environment, Expr(..), parseEnvironment)
import           System.IO        (hSetEncoding, stdin, stdout, utf8)
import           System.Environment (withArgs, getArgs)
import HhiReducer
import Kiselyov
import System.TimeIt
import Text.RawString.QQ
import qualified Data.Bifunctor
import LambdaToSKI (compileBracket)
import TermReducer
import IonAssembly (toIon)
import MicroHsExp (toMhsPrg)
import MhsEval (withMhsContext, eval, run)
import qualified MicroHs.Main as MHS (main)
import System.Process (readProcess)



microHsevalTest :: CL -> IO String
microHsevalTest expr = do
  let prg = toMhsPrg expr
  readProcess "mhseval" [] prg

printGraph :: ST s (STRef s (Graph s)) -> ST s String
printGraph graph = do
  gP <- graph
  toString gP

reduceGraph :: ST s (STRef s (Graph s)) -> ST s (STRef s (Graph s))
reduceGraph graph = do
  gP <- graph
  normalForm gP

main :: IO ()
main = do
  --hSetEncoding stdin utf8 -- this is required to handle UTF-8 characters like λ
  --hSetEncoding stdout utf8 -- this is required to handle UTF-8 characters like λ
  let source = factorial
  let env = parseEnvironment source
  let expr' = compileEta env
  putStrLn $ "Factorial compiled to combinator expression:\n" ++ show expr'

  let prg = toMhsPrg expr'
  putStrLn $ "The resulting MicroHs program: \n" ++ prg

  -- use microHs to compile 'Example.hs' to 'out.comb'
  withArgs ["Example.hs"] MHS.main
  -- read the program 'out.comb' into a string
  prg' <- readFile "out.comb"
  -- use the MicroHs runtime to execute the program
  withMhsContext $ \ctx ->
    run ctx prg'

  -- -- use MicroHs to compile AND execute the Example.hs program
  withArgs ["-r", "Example.hs"] MHS.main
  -- --prg <- readFile "out.comb"
  
  result <- withMhsContext $ \ctx ->
    eval ctx prg
  putStrLn $ "Result: " ++ result
  -- return ()

  -- combCode <- readFile "out.comb"
  -- withMhsContext $ \ctx -> do
  --   putStrLn "Running the compiled program:"
  --   run ctx combCode


  --let testSource = "main = (\\x y -> + x x) 3 4"
  --mapM_ showCompilations [factorial, fibonacci, ackermann, tak]
  --demo

type SourceCode = String

ex :: SourceCode
ex = [r| 

  main = if (eql 0 1) 23 42
|]

prod :: SourceCode
prod = [r| 
  mult = λx y. * y x
  main = mult 3 (+ 5 7)
|]

tak :: SourceCode
tak = [r| 
  tak  = y(λf x y z. (if (geq y x) z (f (f (- x 1) y z) (f (- y 1) z x) (f (- z 1) x y ))))
  main = tak 18 6 3
|]

ackermann :: SourceCode
ackermann = [r|
  ack  = y(λf n m. if (eql n 0) (+ m 1) (if (eql m 0) (f (- n 1) 1) (f (- n 1) (f n (- m 1)))))
  main = ack 3 7
|]

factorial :: SourceCode
factorial = [r| 
  fact = y(\f n. if (eql n 0) 1 (* n (f (- n 1))))
  main = fact 10
|]

fibonacci :: SourceCode
fibonacci = [r| 
  fib  = y(λf n. if (eql n 0) 1 (if (eql n 1) 1 (+ (f (- n 1)) (f (- n 2)))))
  main = fib 31
|]

printMhs :: CL -> IO ()
printMhs cl = do
  --let (n, exps, prg) = toStringCMdl ([], toMhsExp cl)
  putStrLn ("MicroHs expression: " ++ toMhsPrg cl)

printCS :: CL -> IO ()
printCS cl = do
  putStrLn ("ION code: " ++ toIon cl)
  putStrLn ("code size: " ++ show (codeSize cl))

-- data CL = Com Combinator | INT Integer | CL :@ CL
codeSize :: CL -> Int
codeSize (Com _) = 1
codeSize (INT _) = 0
codeSize (t :@ u) = codeSize t + codeSize u

showCompilations :: SourceCode -> IO ()
showCompilations source = do
  let env = parseEnvironment source
  putStrLn "The parsed environment of named lambda expressions:"
  mapM_ print env
  putStrLn ""
  putStrLn "The main expression in de Bruijn notation:"
  mapM_ (print . Data.Bifunctor.second deBruijn) env

  let expr = compileBracket env
  putStrLn "The main expression compiled to SICKBY combinator expressions by recursice bracket abstraction:"
  print expr
  printCS expr
  printMhs expr
  putStrLn ""

  putStrLn "applying plain Kiselyov compilation:"
  print $ compilePlain env
  printCS $ compilePlain env
  printMhs $ compilePlain env
  putStrLn ""

  let exprK = compileK env
  putStrLn "The main expression compiled to SICKBY combinator expressions with K-optimization:"
  print exprK
  printCS exprK
  printMhs exprK
  putStrLn ""


  let expr' = compileEta env
  putStrLn "The main expression compiled to SICKBY combinator expressions with Eta-optimization:"
  print expr'
  printCS expr'
  putStrLn ""


  let expr'' = compileBulk env
  putStrLn "The main expression compiled to SICKBY combinator expressions with bulk combinators:"
  print expr''
  printCS expr''
  putStrLn ""

  let expr''' = compileBulkLinear env
  putStrLn "The main expression compiled to SICKBY combinator expressions with bulk combinators and linear elimination:"
  print expr'''
  printCS expr'''
  putStrLn ""

  let expr'''' = compileBulkLog env
  putStrLn "The main expression compiled to SICKBY combinator expressions with bulk combinators and logarithmic elimination:"
  print expr''''
  printCS expr''''
  putStrLn ""


hhiReductionDemo :: IO CL -> IO ()
hhiReductionDemo ioexpr = do
  expr <- ioexpr
  putStrLn "compiled to CExpr"
  print expr
  let actual = transLink primitives expr
  putStrLn "after graph reduction:"
  print actual


