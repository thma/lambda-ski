module GraphReduction 
(
    run
)
where

import Parser ( Expr(..), parseEnvironment )
import LambdaToSKI (compile)
import Data.Maybe (fromMaybe)

type Pointer = Int

data Graph =
    Node Pointer Pointer
  | Comb String
  | Num  Integer
  deriving (Eq, Show)

type AllocatedGraph = [(Pointer, Graph)]

collectAllReachables :: Pointer -> AllocatedGraph -> AllocatedGraph -> AllocatedGraph
collectAllReachables rootP aGraph result =
  let rootNode = peek rootP aGraph
  in case rootNode of
    Node l r -> (l, peek l aGraph) : (r, peek r aGraph) : collectAllReachables l aGraph result ++ collectAllReachables r aGraph result ++ result
    Comb s -> result
    Num n -> result


compactify :: Pointer -> AllocatedGraph -> AllocatedGraph
compactify rootP aGraph = (rootP, peek rootP aGraph) : collectAllReachables rootP aGraph []


allocate :: Expr -> AllocatedGraph
allocate expr =
  alloc expr 1 []
  where
    alloc :: Expr -> Int -> [(Int, Graph)] -> [(Int, Graph)]
    alloc (Var name) pointer memMap = (pointer, Comb name) : memMap
    alloc (Int val)  pointer memMap = (pointer, Num val) : memMap
    alloc (l :@ r)   pointer memMap =
      let pointerL = pointer+1
          allocL   = alloc l pointerL []
          maxL     = maxPointer allocL
          pointerR = maxL + 1
          allocR   = alloc r pointerR []
          maxR     = maxPointer allocR
      in
      (pointer, Node pointerL pointerR) : (allocL ++ allocR ++ memMap)
    alloc (Lam _ _)  pointer memMap = error "lambdas should already be abstracted"

maxPointer :: AllocatedGraph -> Pointer
maxPointer g = maximum $ map fst g

spine :: Pointer -> AllocatedGraph -> [(Pointer, Graph)] -> (Graph, [(Pointer, Graph)])
spine rootP graph stack =
  case peek rootP graph of
    c@(Comb _)   -> (c, stack)
    n@(Num _)    -> (n, stack)
    g@(Node l r) -> spine l graph ((rootP,g):stack)


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
          return $ snd(head(loop 1 g))

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


apply :: String -> [(Pointer, Graph)] -> AllocatedGraph-> AllocatedGraph
apply "i" ((p,Node _ xP):_) aGraph =
  let xVal = peek xP aGraph
  in  poke p xVal aGraph
apply "k" ((_p, Node _ xP):(p, Node _ _):_) aGraph =
  poke p (peek xP aGraph) aGraph
apply "s" ((_p1, Node _ xP):(_p2, Node _ yP):(p3, Node _ zP):_) aGraph =
  let maxp = maxPointer aGraph
      node1 = (maxp+1, Node xP zP)
      node2 = (maxp+2, Node yP zP)
  in node1 : node2 : poke p3 (Node (maxp+1) (maxp+2)) aGraph
apply "b" ((_p1, Node _ xP):(_p2, Node _ yP):(p3, Node _ zP):_) aGraph =
  let maxp = maxPointer aGraph
      node1 = (maxp+1, Node yP zP)
  in  node1 : poke p3 (Node xP (maxp+1)) aGraph  
apply "c" ((_p1, Node _ xP):(_p2, Node _ yP):(p3, Node _ zP):_) aGraph =
  let maxp = maxPointer aGraph
      node1 = (maxp+1, Node xP zP)
  in  node1 : poke p3 (Node (maxp+1) yP) aGraph
apply "y" ((p1, Node _ fP) : _) aGraph =
  poke p1 (Node fP p1) aGraph

apply "if" ((_p1, Node _ testP):(_p2, Node _ xP):(p3, Node _ yP):_) aGraph =
  case subEval testP aGraph of
    Num test -> if test == 1
                  then poke p3 (peek xP aGraph) aGraph
                  else poke p3 (peek yP aGraph) aGraph
    Comb k   -> error $ "does not evaluate to bool: 0" ++ k
    n@(Node l r) -> error $ "should not happen" ++ show n  

apply "+" ((_p1, Node _ xP) : (p2, Node _ yP):_) aGraph = 
  let (Num x) = subEval xP aGraph
      (Num y) = subEval yP aGraph
      result  = x + y
  in poke p2 (Num result) aGraph
apply "*" ((_p1, Node _ xP) : (p2, Node _ yP):_) aGraph = 
  let (Num x) = subEval xP aGraph
      (Num y) = subEval yP aGraph
      result  = x * y
  in poke p2 (Num result) aGraph
apply "sub" ((_p1, Node _ xP) : (p2, Node _ yP):_) aGraph = 
  let (Num x) = subEval xP aGraph
      (Num y) = subEval yP aGraph
      result  = x - y
  in poke p2 (Num result) aGraph
apply "/" ((_p1, Node _ xP) : (p2, Node _ yP):_) aGraph = 
  let (Num x) = subEval xP aGraph
      (Num y) = subEval yP aGraph
      result  = x `div` y
  in poke p2 (Num result) aGraph

apply "eq" ((_p1, Node _ xP) : (p2, Node _ yP):_) aGraph = 
  let x = subEval xP aGraph
      y = subEval yP aGraph
      result  = if x == y then 1 else 0
  in poke p2 (Num result) aGraph

apply k _ _  = error $ "undefined combinator " ++ k 

subEval :: Pointer -> AllocatedGraph -> Graph
subEval p g = snd ( head (loop p g))

  --  |apply (I,(node as ref(app((_,ref x),_,ref q)))::_) =
  --       (node := x; set_q node q)
  --  |apply (K,ref(app((_,ref x),_,ref q))::(node as ref(app(_,_,_)))::_) =
  --       (node := x; set_q node q)
  --  |apply (S,(ref(app((_,x),_,_)))::(ref(app((_,y),_,_)))
  --             ::(node as (ref(app((_,z),m,q))))::_) =
  --       node := app((ref(app((x,z),ref Eval,q)),
  --                    ref(app((y,z),ref Eval,q))),
  --                   ref Eval,q)
  --  |apply (B,(ref(app((_,x),_,_)))::(ref(app((_,y),_,_)))
  --             ::(node as (ref(app((_,z),m,q))))::_) =
  --       node := app((x,ref (app((y,z),ref Eval,q))),ref Eval,q)
  --  |apply (C,(ref(app((_,x),_,_)))::(ref(app((_,y),_,_)))
  --             ::(node as (ref(app((_,z),m,q))))::_) =
  --       node := app((ref(app((x,z),ref Eval,q)),y),ref Eval,q)

  --  |apply (Y,(node as ref(app((_,f),m,q)))::_) =
  --       node := app((f,node),ref Eval,q)
  --  |apply (DEF(name),(node as ref(app((_,_),_,_)))::_) =
  --       node := !(copy(lookup name))
  --  |apply (PLUS,ref(app((_,ref(atom(int x,_,_))),_,_))::(node as 
  --               ref(app((_,ref(atom(int y,_,_))),_,q)))::_) =
  --       node := atom(int(x+y),ref Ready,q)
  --  |apply (PLUS,(stack as ref(app((_,x),_,_))::
  --                         ref(app((_,y),_,_))::_)) =
  --       (subEval (last stack,x);
  --        subEval (last stack,y); ())
  --  |apply (MINUS,ref(app((_,ref(atom(int x,_,_))),_,_))::(node as 
  --                ref(app((_,ref(atom(int y,_,_))),_,q)))::_) =
  --       node := atom(int(x-y),ref Ready,q)
  --  |apply (MINUS,(stack as ref(app((_,x),_,_))::
  --                         ref(app((_,y),_,_))::_)) =
  --       (subEval (last stack,x);
  --        subEval (last stack,y); ())
  --  |apply (TIMES,ref(app((_,ref(atom(int x,_,_))),_,_))::(node as 
  --                ref(app((_,ref(atom(int y,_,_))),_,q)))::_) =
  --       node := atom(int(x*y),ref Ready,q)
  --  |apply (TIMES,(stack as ref(app((_,x),_,_))::
  --                         ref(app((_,y),_,_))::_)) =
  --       (subEval (last stack,x);
  --        subEval (last stack,y); ())
  --  |apply (DIV,ref(app((_,ref(atom(int x,_,_))),_,_))::(node as 
  --              ref(app((_,ref(atom(int y,_,_))),_,q)))::_) =
  --       node := atom(int(x div y),ref Ready,q)
  --  |apply (DIV,(stack as ref(app((_,x),_,_))::
  --                         ref(app((_,y),_,_))::_)) =
  --       (subEval (last stack,x);
  --        subEval (last stack,y); ())
  --  |apply (EQ,(stack as ref(app((_,x),_,_))::(node as
  --                         ref(app((_,y),_,q)))::_)) =
  --       if (!(get_mark x)) = Ready andalso
  --          (!(get_mark y)) = Ready 
  --         then node := atom(bool(equal x y),ref Ready,q)
  --       else
  --         (subEval (last stack,x);
  --         subEval (last stack,y); ())
  --  |apply (IF,(ref(app((_,ref(atom(bool test,_,_))),_,_))):: 
  --             (ref(app((_,x),_,_)))::(node as (ref(app((_,y),_,_))))::_) =
  --       if test then node := !x
  --       else node := !y
  --  |apply (IF,(stack as (ref(app((_,test),_,_)):: 
  --             ref(app((_,x),_,_))::(node as ref(app((_,y),_,q)))::_))) =
  --       subEval (last stack,test)


peek :: Pointer -> AllocatedGraph -> Graph
peek pointer graph = fromMaybe (error "merde") (lookup pointer graph)

poke :: Pointer -> Graph -> AllocatedGraph -> AllocatedGraph
poke key value assoc = (key,value):filter ((key /=).fst) assoc
