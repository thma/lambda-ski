module GraphReduction
  ( toString,
    mToString,
    step,
    allocate,
    spine,
    normalForm,
    nf,
    Graph (..),
    copy,
  )
where

import           Control.Monad ( (<=<) )
import           Control.Monad.ST (ST)
import           Data.STRef       (STRef, newSTRef, readSTRef,
                                   writeSTRef)
import           Parser           (Expr (..))
import CLTerm


infixl 5 :@:

data Graph s
  = (STRef s (Graph s)) :@: (STRef s (Graph s))
  | Comb Combinator
  | Num Integer
  deriving (Eq)


copy :: STRef s (Graph s) -> ST s (STRef s (Graph s))
copy graph = do
  g <- readSTRef graph
  copy' g
  where
    copy' (Comb c) = newSTRef (Comb c)
    copy' (Num n) = newSTRef (Num n)
    copy' (lP :@: rP) = do
      lG <- readSTRef lP
      rG <- readSTRef rP
      lP' <- newSTRef lG
      rP' <- newSTRef rG
      newSTRef (lP' :@: rP')

toString :: STRef s (Graph s) -> ST s String
toString graph = do
  g <- readSTRef graph
  toString' g
  where
    toString' (Comb c) = return $ show c
    toString' (Num i) = return $ show i
    toString' (lP :@: rP) = do
      lG <- readSTRef lP
      rG <- readSTRef rP
      lStr <- toString' lG
      rStr <- toString' rG
      return $ "(" ++ lStr ++ " :@: " ++ rStr ++ ")"

mToString :: ST s (STRef s (Graph s)) -> ST s String
mToString g = toString =<< g

allocate :: CL -> ST s (STRef s (Graph s))
allocate (Com c) = newSTRef $ Comb c
allocate (INT i) = newSTRef $ Num i
allocate (l :@ r) = do
  lg <- allocate l
  rg <- allocate r
  newSTRef $ lg :@: rg

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

step :: STRef s (Graph s) -> ST s ()
step graph = do
  (top : stack) <- spine graph
  node <- readSTRef top
  case node of
    (Comb k) -> reduce k stack
    _        -> return ()

normalForm :: STRef s (Graph s) -> ST s (STRef s (Graph s))
normalForm graph = do
  before <- readSTRef graph
  step graph
  after <- readSTRef graph
  g <- readSTRef graph
  case g of
    _lP :@: _rP -> normalForm graph -- if before == after then return graph else normalForm graph
    Comb _com   -> return graph
    Num _n      -> return graph

nf :: STRef s (Graph s) -> ST s [String]
nf g = nf' g []

nf' :: STRef s (Graph s) -> [String] -> ST s [String]
nf' graph l = do
  before <- toString graph
  step graph
  after <- toString graph
  let steplist = l ++ [after]
  g <- readSTRef graph
  case g of
    _lP :@: _rP -> if before == after then return steplist else nf' graph steplist
    Comb _com -> return steplist
    Num _n -> return steplist

reduce :: Combinator -> LeftAncestorsStack s -> ST s ()
reduce I (p : _) = do
  (_ :@: xP) <- readSTRef p
  xVal <- readSTRef xP
  writeSTRef p xVal
reduce K (p1 : p2 : _) = do
  (_ :@: xP) <- readSTRef p1
  xVal <- readSTRef xP
  writeSTRef p2 xVal
reduce S (p1 : p2 : p3 : _) = do
  (_ :@: xP) <- readSTRef p1
  (_ :@: yP) <- readSTRef p2
  (_ :@: zP) <- readSTRef p3
  node1 <- newSTRef $ xP :@: zP
  node2 <- newSTRef $ yP :@: zP
  writeSTRef p3 (node1 :@: node2)
reduce B (p1 : p2 : p3 : _) = do
  (_ :@: xP) <- readSTRef p1
  (_ :@: yP) <- readSTRef p2
  (_ :@: zP) <- readSTRef p3
  node1 <- newSTRef $ yP :@: zP
  writeSTRef p3 (xP :@: node1)
reduce C (p1 : p2 : p3 : _) = do
  (_ :@: xP) <- readSTRef p1
  (_ :@: yP) <- readSTRef p2
  (_ :@: zP) <- readSTRef p3
  node1 <- newSTRef $ xP :@: zP
  writeSTRef p3 (node1 :@: yP)

-- B' P Q R S = P Q (R S)
reduce B' (p : q : r : s : _) = do
  (_ :@: pP) <- readSTRef p
  (_ :@: qP) <- readSTRef q
  (_ :@: rP) <- readSTRef r
  (_ :@: sP) <- readSTRef s
  node1 <- newSTRef $ pP :@: qP
  node2 <- newSTRef $ rP :@: sP
  writeSTRef s (node1 :@: node2)

-- C' P Q R S = P (Q S) R
reduce C' (p : q : r : s : _) = do
  (_ :@: pP) <- readSTRef p
  (_ :@: qP) <- readSTRef q
  (_ :@: rP) <- readSTRef r
  (_ :@: sP) <- readSTRef s
  node1 <- newSTRef $ qP :@: sP
  node2 <- newSTRef $ pP :@: node1
  writeSTRef s (node2 :@: rP) 

-- S' P Q R S = P (Q S) (R S)
reduce S' (p : q : r : s : _) = do
  (_ :@: pP) <- readSTRef p
  (_ :@: qP) <- readSTRef q
  (_ :@: rP) <- readSTRef r
  (_ :@: sP) <- readSTRef s
  node1 <- newSTRef $ qP :@: sP
  node2 <- newSTRef $ pP :@: node1
  node3 <- newSTRef $ rP :@: sP
  writeSTRef s (node2 :@: node3)  

reduce Y (p1 : _) = do
  (_yP :@: fP) <- readSTRef p1
  writeSTRef p1 (fP :@: p1)
reduce IF (p1 : p2 : p3 : _) = do
  (_ :@: testP) <- readSTRef p1
  (_ :@: xP) <- readSTRef p2
  (_ :@: yP) <- readSTRef p3
  test <- (readSTRef <=< normalForm) testP
  thenPart <- readSTRef xP
  elsePart <- readSTRef yP
  case test of
    (_ :@: _) -> error "does not eval to bool"
    Comb comb -> error $ "does not eval to bool: " ++ show comb
    Num n ->
      if n == 1
        then writeSTRef p3 thenPart
        else writeSTRef p3 elsePart
reduce ADD (p1 : p2 : _) = binaryMathOp (+) p1 p2
reduce MUL (p1 : p2 : _) = binaryMathOp (*) p1 p2
reduce DIV (p1 : p2 : _) = binaryMathOp div p1 p2
reduce SUB (p1 : p2 : _) = binaryMathOp (-) p1 p2
reduce REM (p1 : p2 : _) = binaryMathOp rem p1 p2
reduce EQL (p1 : p2 : _) = binaryMathOp binOpEql p1 p2
reduce GEQ (p1 : p2 : _) = binaryMathOp binOpGeq p1 p2
reduce SUB1 (p1 : _) = do
  (_ :@: xP) <- readSTRef p1
  x <- normalForm xP
  (Num xVal) <- readSTRef x
  writeSTRef p1 (Num $ xVal - 1)
reduce ZEROP (p1 : _) = do
  (_ :@: xP) <- readSTRef p1
  (Num xVal) <- (readSTRef <=< normalForm) xP
  let result = if xVal == 0 then 1 else 0
  writeSTRef p1 (Num result)
reduce _ _ = return ()

binaryMathOp ::
  (Integer -> Integer -> Integer) ->
  STRef s (Graph s) ->
  STRef s (Graph s) ->
  ST s ()
binaryMathOp op p1 p2 = do
  (_ :@: xP) <- readSTRef p1
  (_ :@: yP) <- readSTRef p2
  (Num xVal) <- (readSTRef <=< normalForm) xP
  (Num yVal) <- (readSTRef <=< normalForm) yP
  writeSTRef p2 (Num $ xVal `op` yVal)

binOpEql :: Integer -> Integer -> Integer 
binOpEql x y = if x == y then 1 else 0

binOpGeq :: Integer -> Integer -> Integer 
binOpGeq x y = if x >= y then 1 else 0