{-# LANGUAGE LambdaCase #-}
module Kiselyov
  (
    deBruijn,
    bulkOpt,
    compilePlain,
    compileK,
    compileEta,
    compileBulk,
    compileBulkLinear,
    compileBulkLog,
    optK,
    optEta
  )
where
import Parser
import CLTerm

{--
This is almost a verbatim copy of the Kiselyov compiler from B. Lynn's exposition of Kiselyov's bracket abstraction
https://crypto.stanford.edu/~blynn/lambda/kiselyov.html.

I've only added minor changes to fit it into my codebase.
E.g. I've added access to the environment of named lambda expressions for free variables.
--}

data Peano = Succ Peano | Zero deriving Show
data DB = N Peano | L DB | A DB DB | Free String | IN Integer deriving Show

deBruijn :: Expr -> DB
deBruijn = go [] where
  go binds = \case
    Var x -> maybe (Free x) N $ index x binds
    Lam x t -> L $ go (x:binds) t
    App t u -> A (go binds t) (go binds u)
    Int i -> IN i

index :: Eq a => a -> [a] -> Maybe Peano
index x xs = lookup x $ zip xs $ iterate Succ Zero

convert :: ((Int, CL) -> (Int, CL) -> CL) -> [(String, Expr)] -> DB -> (Int, CL)
convert (#) env = \case
  N Zero -> (1, Com I)
  N (Succ e) -> (n + 1, (0, Com K) # t) where t@(n, _) = rec $ N e
  L e -> case rec e of
    (0, d) -> (0, Com K :@ d)
    (n, d) -> (n - 1, d)
  A e1 e2 -> (max n1 n2, t1 # t2) where
    t1@(n1, _) = rec e1
    t2@(n2, _) = rec e2
  IN i -> (0, INT i)
  Free s -> convertVar (#) env s
  where rec = convert (#) env

-- | convert a free variable to a combinator.
--   first we try to find a definition in the environment.
--   if that fails, we assume it is a SICKBY combinator.
convertVar :: ((Int, CL) -> (Int, CL) -> CL) -> [(String, Expr)] -> String -> (Int, CL)
convertVar (#) env s
  | Just t <- lookup s env = convert (#) env (deBruijn t)
  | otherwise = (0, Com (fromString s))

plain :: Environment -> DB -> (Int, CL)
plain = convert (#) where
  (0 , d1) # (0 , d2) = d1 :@ d2
  (0 , d1) # (n , d2) = (0, Com B :@ d1) # (n - 1, d2)
  (n , d1) # (0 , d2) = (0, Com R :@ d2) # (n - 1, d1)
  (n1, d1) # (n2, d2) = (n1 - 1, (0, Com S) # (n1 - 1, d1)) # (n2 - 1, d2)

compilePlain :: Environment -> CL
compilePlain env = case lookup "main" env of
  Nothing   -> error "main function missing"
  Just main -> snd $ plain env (deBruijn main)

bulk :: Combinator -> Int -> CL
bulk c 1 = Com c
bulk c n = Com $ BulkCom (show c) n

compileK :: Environment -> CL
compileK env = case lookup "main" env of
  Nothing   -> error "main function missing"
  Just main -> snd $ optK env (deBruijn main)

compileEta :: Environment -> CL
compileEta env = case lookup "main" env of
  Nothing   -> error "main function missing"
  Just main -> snd $ optEta env (deBruijn main)

compileBulk :: Environment -> CL
compileBulk env = case lookup "main" env of
  Nothing   -> error "main function missing"
  Just main -> snd $ bulkOpt bulk env (deBruijn main)

compileBulkLinear :: Environment -> CL
compileBulkLinear env = case lookup "main" env of
  Nothing   -> error "main function missing"
  Just main -> snd $ bulkOpt breakBulkLinear env (deBruijn main)

compileBulkLog :: Environment -> CL
compileBulkLog env = case lookup "main" env of
  Nothing   -> error "main function missing"
  Just main -> snd $ bulkOpt breakBulkLog env (deBruijn main)

convertBool :: (([Bool], CL) -> ([Bool], CL) -> CL) -> Environment -> DB -> ([Bool], CL)
convertBool (#) env = \case
  N Zero -> ([True], Com I)
  N (Succ e) -> (False:g, d) where (g, d) = rec env (N e)
  L e -> case rec env e of
    ([], d) -> ([], Com K :@ d)
    (False:g, d) -> (g, ([], Com K) # (g, d))
    (True:g, d) -> (g, d)
  A e1 e2 -> (zipWithDefault False (||) g1 g2, t1 # t2) where
    t1@(g1, _) = rec env e1
    t2@(g2, _) = rec env e2
  Free fun -> convertFree (#) env fun
  IN i -> ([False], INT i)
  where rec = convertBool (#)

-- | convert a free variable to a combinator.
--   first we try to find a definition in the environment.
--   if that fails, we assume it is a SICKBY combinator.
convertFree :: (([Bool], CL) -> ([Bool], CL) -> CL) -> [(String, Expr)] -> String -> ([Bool], CL)
convertFree (#) env s
  | Just t <- lookup s env = convertBool (#) env (deBruijn t)
  | otherwise = ([], Com (fromString s))

optK :: Environment -> DB -> ([Bool], CL)
optK = convertBool (#) where
  ([], d1) # ([], d2) = d1 :@ d2
  ([], d1) # (True:g2, d2) = ([], Com B :@ d1) # (g2, d2)
  ([], d1) # (False:g2, d2) = ([], d1) # (g2, d2)
  (True:g1, d1) # ([], d2) = ([], Com R :@ d2) # (g1, d1)
  (False:g1, d1) # ([], d2) = (g1, d1) # ([], d2)
  (True:g1, d1) # (True:g2, d2) = (g1, ([], Com S) # (g1, d1)) # (g2, d2)
  (False:g1, d1) # (True:g2, d2) = (g1, ([], Com B) # (g1, d1)) # (g2, d2)
  (True:g1, d1) # (False:g2, d2) = (g1, ([], Com C) # (g1, d1)) # (g2, d2)
  (False:g1, d1) # (False:g2, d2) = (g1, d1) # (g2, d2)

optEta :: Environment -> DB -> ([Bool], CL)
optEta = convertBool (#) where
  ([], d1) # ([], d2) = d1 :@ d2
  ([], d1) # (True:[], Com I) = d1
  ([], d1) # (True:g2, d2) = ([], Com B :@ d1) # (g2, d2)
  ([], d1) # (False:g2, d2) = ([], d1) # (g2, d2)
  (True:[], Com I) # ([], d2) = Com T :@ d2
  (True:[], Com I) # (False:g2, d2) = ([], Com T) # (g2, d2)
  (True:g1, d1) # ([], d2) = ([], Com R :@ d2) # (g1, d1)
  (True:g1, d1) # (True:g2, d2) = (g1, ([], Com S) # (g1, d1)) # (g2, d2)
  (True:g1, d1) # (False:g2, d2) = (g1, ([], Com C) # (g1, d1)) # (g2, d2)
  (False:g1, d1) # ([], d2) = (g1, d1) # ([], d2)
  (False:g1, d1) # (True:[], Com I) = d1
  (False:g1, d1) # (True:g2, d2) = (g1, ([], Com B) # (g1, d1)) # (g2, d2)
  (False:g1, d1) # (False:g2, d2) = (g1, d1) # (g2, d2)

zipWithDefault :: t -> (t -> t -> b) -> [t] -> [t] -> [b]
zipWithDefault d f     []     ys = f d <$> ys
zipWithDefault d f     xs     [] = flip f d <$> xs
zipWithDefault d f (x:xt) (y:yt) = f x y : zipWithDefault d f xt yt

bulkLookup :: String -> Environment -> (Combinator -> Int -> CL) -> ([Bool], CL)
bulkLookup s env bulkFun = case lookup s env of
  Nothing -> ([], Com (fromString s))
  Just t -> bulkOpt bulkFun env (deBruijn t)

bulkOpt :: (Combinator -> Int -> CL) -> Environment -> DB -> ([Bool], CL)
bulkOpt bulkFun env = \case
  N Zero -> ([True], Com I)
  N (Succ e) -> first (False:) $ rec env $ N e
  L e -> case rec env e of
    ([], d) -> ([], Com K :@ d)
    (False:g, d) -> ([], Com K) ## (g, d)
    (True:g, d) -> (g, d)
  A e1 e2 -> rec env e1 ## rec env e2
  Free s -> bulkLookup s env bulkFun--([], Com s)
  IN i -> ([False], INT i)
  where
  rec = bulkOpt bulkFun
  ([], d1) ## ([], d2) = ([], d1 :@ d2)
  ([], d1) ## ([True], Com I) = ([True], d1)
  ([], d1) ## (g2, Com I) | and g2 = (g2, bulkFun B (length g2 - 1) :@ d1)
  ([], d1) ## (g2@(h:_), d2) = first (pre++) $ ([], fun1 d1) ## (post, d2)
    where
    fun1 = case h of
      True -> (bulkFun B (length pre) :@)
      False -> id
    (pre, post) = span (h ==) g2

  ([True], Com I) ## ([], d2) = ([True], Com T :@ d2)
  (g1@(h:_), d1) ## ([], d2) = first (pre++) $ case h of
    True -> ([], Com C :@ bulkFun C (length pre) :@ d2) ## (post, d1)
    False -> (post, d1) ## ([], d2)
    where
    (pre, post) = span (h ==) g1

  ([True], Com I) ## (False:g2, d2) = first (True:) $ ([], Com T) ## (g2, d2)
  (False:g1, d1) ## ([True], Com I) = (True:g1, d1)
  (g1, d1) ## (g2, Com I) | and g2, let n = length g2, all not $ take n g1 =
    first (g2++) $ ([], bulkFun B $ n - 1) ## (drop n g1, d1)
  (g1, d1) ## (g2, d2) = pre $ fun1 (drop count g1, d1) ## (drop count g2, d2)
    where
    (h, count) = headGroup $ zip g1 g2
    fun1 = case h of
      (False, False) -> id
      (False, True) -> apply B
      (True, False) -> apply C
      (True, True) -> apply S
    pre = first (replicate count (uncurry (||) h) ++)
    apply s = (([], bulkFun s count) ##)

first :: (t -> a) -> (t, b) -> (a, b)
first f (x, y) = (f x, y);

headGroup :: Eq a => [a] -> (a, Int)
headGroup (h:t) = (h, 1 + length (takeWhile (== h) t))

breakBulkLinear :: Combinator -> Int -> CL
breakBulkLinear B n = iterate (comB' :@) (Com B) !! (n - 1)
breakBulkLinear C n = iterate (comC' :@) (Com C) !! (n - 1)
breakBulkLinear S n = iterate (comS' :@) (Com S) !! (n - 1)

comB' :: CL
comB' = Com B:@ Com B
comC' :: CL
comC' = Com B :@ (Com B :@ Com C) :@ Com B
comS' :: CL
comS' = Com B :@ (Com B :@ Com S) :@ Com B

breakBulkLog :: Combinator -> Int -> CL
breakBulkLog c 1 = Com c
breakBulkLog B n = foldr (:@) (Com B) $ map (bs!!) $ init $ bits n where
  bs = [sbi, Com B :@ (Com B :@ Com B) :@ sbi]
breakBulkLog c n = (foldr (:@) (prime c) $ map (bs!!) $ init $ bits n) :@ Com I where
  bs = [sbi, Com B :@ (Com B :@ prime c) :@ sbi]
  prime c = Com B :@ (Com B :@ Com c) :@ Com B

bits :: Int -> [Int]
bits n = r:if q == 0 then [] else bits q where (q, r) = divMod n 2

sbi :: CL
sbi = Com S :@ Com B :@ Com I
