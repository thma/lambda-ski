{-# LANGUAGE QuasiQuotes #-}
module SimpleTestSources where

import Text.RawString.QQ  

type SourceCode = String

-- Very simple test cases for TermReducer
basicArithmetic :: SourceCode
basicArithmetic = [r| 
  expected = 7
  main = + 3 4
|]

basicComparison :: SourceCode
basicComparison = [r| 
  expected = k
  main = eql 5 5
|]

basicBoolean :: SourceCode
basicBoolean = [r| 
  expected = 5
  main = if (is0 0) 5 7
|]

basicSubtraction :: SourceCode
basicSubtraction = [r| 
  expected = 2
  main = - 5 3
|]

basicMultiplication :: SourceCode
basicMultiplication = [r| 
  expected = 15
  main = * 3 5
|]
