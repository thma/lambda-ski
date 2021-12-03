module GraphReduction 
(
    run
)
where

import Parser ( Expr(..), parseEnvironment )
import LambdaToSKI (compile)
import Data.Maybe (fromMaybe)

type Pointer = Int

infixl 5 :@:
data Graph =
    Pointer :@: Pointer
  | Comb Combinator
  | Num  Integer
  deriving (Eq, Show)

data Combinator = I | K | S | B | C | Y | P | ADD | SUB | MUL | DIV | REM | SUB1 | EQL | ZEROP | IF
                  deriving (Eq, Show)

fromString :: String -> Combinator
fromString "i" = I
fromString "k" = K
fromString "s" = S
fromString "b" = B
fromString "c" = C
fromString "y" = Y
fromString "p" = P
fromString "+" = ADD
fromString "sub" = SUB
fromString "div" = DIV
fromString "rem" = REM
fromString "*" = MUL
fromString "sub1" = SUB1
fromString "eq" = EQL
fromString "is0" = ZEROP
fromString "if" = IF
fromString _c = error $ "unknown combinator " ++ _c

type AllocatedGraph = [(Pointer, Graph)]

collectAllReachables :: Pointer -> AllocatedGraph -> AllocatedGraph -> AllocatedGraph
collectAllReachables rootP aGraph result =
  let rootNode = peek rootP aGraph
  in case rootNode of
    l :@: r -> (l, peek l aGraph) : (r, peek r aGraph) : collectAllReachables l aGraph result ++ collectAllReachables r aGraph result ++ result
    Comb s -> result
    Num n -> result

compactify :: Pointer -> AllocatedGraph -> AllocatedGraph
compactify rootP aGraph = (rootP, peek rootP aGraph) : collectAllReachables rootP aGraph []

allocate :: Expr -> AllocatedGraph
allocate expr =
  alloc expr 1 []
  where
    alloc :: Expr -> Int -> [(Int, Graph)] -> [(Int, Graph)]
    alloc (Var name) pointer memMap = (pointer, Comb $ fromString name) : memMap
    alloc (Int val)  pointer memMap = (pointer, Num val) : memMap
    alloc (l :@ r)   pointer memMap =
      let pointerL = pointer+1
          allocL   = alloc l pointerL []
          maxL     = maxPointer allocL
          pointerR = maxL + 1
          allocR   = alloc r pointerR []
          maxR     = maxPointer allocR
      in
      (pointer, pointerL :@: pointerR) : (allocL ++ allocR ++ memMap)
    alloc (Lam _ _)  pointer memMap = error "lambdas should already be abstracted"

maxPointer :: AllocatedGraph -> Pointer
maxPointer g = maximum $ map fst g

spine :: Pointer -> AllocatedGraph -> [(Pointer, Graph)] -> (Graph, [(Pointer, Graph)])
spine rootP graph stack =
  case peek rootP graph of
    c@(Comb _)   -> (c, stack)
    n@(Num _)    -> (n, stack)
    g@(l :@: r) -> spine l graph ((rootP,g):stack)


run :: String -> IO Graph
run source =
  case parseEnvironment source of
      Left err -> error $ show err
      Right env -> case compile env of
        Left err -> error $ show err
        Right skiExpr -> do
          let g = allocate skiExpr
          print skiExpr
          print g
          print (spine 1 g [])
          let result = snd(head(loop 1 g))
          print result
          return result

loop :: Pointer -> AllocatedGraph -> AllocatedGraph
loop rootP aGraph =
  let aGraph' = compactify rootP (step rootP aGraph)
  in  if aGraph == aGraph' 
        then aGraph
        else loop rootP aGraph'

step :: Pointer -> AllocatedGraph -> AllocatedGraph
step rootP graph =
  let root = peek rootP
      (g,stack)   = spine rootP graph []
  in  case g of
        (Comb k) -> apply k stack graph
        _        -> graph


apply :: Combinator -> [(Pointer, Graph)] -> AllocatedGraph-> AllocatedGraph
apply I ((p,_ :@: xP):_) aGraph =
  let xVal = peek xP aGraph
  in  poke p xVal aGraph
apply K ((_p, _ :@: xP):(p, _ :@: _):_) aGraph =
  poke p (peek xP aGraph) aGraph
apply S ((_p1, _ :@: xP):(_p2, _ :@: yP):(p3, _ :@: zP):_) aGraph =
  let maxp = maxPointer aGraph
      node1 = (maxp+1, xP :@: zP)
      node2 = (maxp+2, yP :@: zP)
  in node1 : node2 : poke p3 ((maxp+1) :@: (maxp+2)) aGraph
apply B ((_p1, _ :@: xP):(_p2, _ :@: yP):(p3, _ :@: zP):_) aGraph =
  let maxp = maxPointer aGraph
      node1 = (maxp+1, yP :@: zP)
  in  node1 : poke p3 (xP :@: (maxp+1)) aGraph  
apply C ((_p1, _ :@: xP):(_p2, _ :@: yP):(p3, _ :@: zP):_) aGraph =
  let maxp = maxPointer aGraph
      node1 = (maxp+1, xP :@: zP)
  in  node1 : poke p3 ((maxp+1) :@: yP) aGraph
apply Y ((p1, yP :@: fP) : _) aGraph =
  -- copying version of Y-combinator works nicely
  let maxp = maxPointer aGraph
      node1 = (maxp+1, yP :@: fP)
  in  node1 : poke p1 (fP :@: (maxp+1)) aGraph
  -- the not-copying version does not yet work, sigh...
  --poke p1 (fP :@: p1) aGraph
{--
|apply (Y,(node as ref(app((_,f),m,q)))::_) =
        node := app((f,node),ref Eval,q)
--}

apply IF ((_p1, _ :@: testP):(_p2, _ :@: xP):(p3, _ :@: yP):_) aGraph =
  case subEval testP aGraph of
    Num test -> if test == 1
                  then poke p3 (peek xP aGraph) aGraph
                  else poke p3 (peek yP aGraph) aGraph
    Comb k   -> error $ "does not evaluate to bool: 0" ++ show k
    n@(l :@: r) -> error $ "should not happen" ++ show n  
apply ADD ((_p1, _ :@: xP) : (p2, _ :@: yP):_) aGraph = 
  let (Num x) = subEval xP aGraph
      (Num y) = subEval yP aGraph
      result  = x + y
  in poke p2 (Num result) aGraph
apply MUL ((_p1, _ :@: xP) : (p2, _ :@: yP):_) aGraph = 
  let (Num x) = subEval xP aGraph
      (Num y) = subEval yP aGraph
      result  = x * y
  in poke p2 (Num result) aGraph
apply SUB ((_p1, _ :@: xP) : (p2, _ :@: yP):_) aGraph = 
  let (Num x) = subEval xP aGraph
      (Num y) = subEval yP aGraph
      result  = x - y
  in poke p2 (Num result) aGraph
apply SUB1 ((p1, _ :@: xP) : _) aGraph = 
  let (Num x) = subEval xP aGraph
  in poke p1 (Num $ x-1) aGraph  
apply DIV ((_p1, _ :@: xP) : (p2, _ :@: yP):_) aGraph = 
  let (Num x) = subEval xP aGraph
      (Num y) = subEval yP aGraph
      result  = x `div` y
  in poke p2 (Num result) aGraph

apply EQL ((_p1, _ :@: xP) : (p2, _ :@: yP):_) aGraph = 
  let x = subEval xP aGraph
      y = subEval yP aGraph
      result  = if x == y then 1 else 0
  in poke p2 (Num result) aGraph
apply ZEROP ((p1, _ :@: xP) :_) aGraph = 
  let x = subEval xP aGraph
      result  = if x == Num 0 then 1 else 0
  in poke p1 (Num result) aGraph

apply k _ aGraph  = aGraph --error $ "undefined combinator " ++ k 

subEval :: Pointer -> AllocatedGraph -> Graph
subEval p g = snd ( head (loop p g))

peek :: Pointer -> AllocatedGraph -> Graph
peek pointer graph = fromMaybe (error "merde") (lookup pointer graph)

poke :: Pointer -> Graph -> AllocatedGraph -> AllocatedGraph
poke key value assoc = (key,value):filter ((key /=).fst) assoc
