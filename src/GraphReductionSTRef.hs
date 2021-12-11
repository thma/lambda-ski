module GraphReductionSTRef
  ( 
    toString,
    step,
    allocate,
    spine,
    normalForm,
    Graph (..),
  )
where

import           Control.Monad    (forM_, (<=<))
import           Control.Monad.ST (ST, runST)
import           Data.STRef       (STRef, modifySTRef, newSTRef, readSTRef,
                                   writeSTRef)
import           LambdaToSKI      (compile, abstractToSKI)
import           Parser           (Expr (..))

-- this just demonstrates the basic functiong of ST and STRef
sumST :: Num a => [a] -> a
sumST xs = runST $ do
  -- runST takes stateful ST code and makes it pure.
  summed <- newSTRef 0 -- Create an STRef (a mutable variable)
  forM_ xs $ \x ->
    modifySTRef summed (+ x) -- add it to what we have in n.
  readSTRef summed -- read the value of n, which will be returned by the runST above.

infixl 5 :@:

data Graph s
  = (STRef s (Graph s)) :@: (STRef s (Graph s))
  | Comb Combinator
  | Num Integer
  deriving (Eq)

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

data Combinator = I | K | S | B | C | Y | P | ADD | SUB | MUL | DIV | REM | SUB1 | EQL | ZEROP | IF
  deriving (Eq, Show)

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

allocate :: Expr -> ST s (STRef s (Graph s))
allocate (Var name) = newSTRef $ Comb $ fromString name
allocate (Int val)  = newSTRef $ Num val
allocate (l :@ r)   = do
  lg <- allocate l
  rg <- allocate r
  newSTRef $ lg :@: rg
allocate (Lam _ _)  = error "lambdas must already be abstracted away!"

spine :: STRef s (Graph s) -> ST s (Graph s, [STRef s (Graph s)])
spine graph = spine' graph [] 
  where
    spine' :: STRef s (Graph s) -> [STRef s (Graph s)] -> ST s (Graph s, [STRef s (Graph s)])
    spine' graph stack = do
      g <- readSTRef graph
      case g of
        c@(Comb _)  -> return (c, stack)
        n@(Num _)   -> return (n, stack)
        (l :@: _r)  -> spine' l (graph : stack)

step :: STRef s (Graph s) -> ST s ()
step graph = do
  (g, stack) <- spine graph
  case g of
    (Comb k) -> reduce k stack
    _        -> return ()

normalForm :: STRef s (Graph s) -> ST s (STRef s (Graph s))
normalForm graph = do
  spine1 <- spine graph
  step graph
  spine2 <- spine graph
  g <- readSTRef graph
  case g of
    _lP :@: _rP -> normalForm graph --if spine1 == spine2 then return graph else normalForm graph
    Comb _com   -> return graph
    Num _n      -> return graph


reduce :: Combinator -> [STRef s (Graph s)] -> ST s ()
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
reduce SUB1 (p1 : _) = do
  (_ :@: xP) <- readSTRef p1
  x <- normalForm xP
  (Num xVal) <- readSTRef x
  writeSTRef p1 (Num $ xVal - 1)
reduce EQL (p1 : p2 : _) = do
  (_ :@: xP) <- readSTRef p1
  (_ :@: yP) <- readSTRef p2
  (Num xVal) <- (readSTRef <=< normalForm) xP
  (Num yVal) <- (readSTRef <=< normalForm) yP
  let result = if xVal == yVal then 1 else 0
  writeSTRef p2 (Num result)
reduce ZEROP (p1 : _) = do
  (_ :@: xP) <- readSTRef p1
  (Num xVal) <- (readSTRef <=< normalForm) xP
  let result = if xVal == 0 then 1 else 0
  writeSTRef p1 (Num result)
reduce _ _ = return ()

-- normalForm :: STRef s (Graph s) -> ST s (STRef s (Graph s))
-- normalForm g = do
--   _ <- normalForm g
--   return g

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
