{-# LANGUAGE LambdaCase #-}
module Kiselyov
  (
    deBruijn,
    convert,
    plain,
    bulkPlain,
    bulk,
    compileKi,
    compileKiEither
  ) 
where
import Parser
import CLTerm

data Peano = Su Peano | Z deriving Show
data DB = N Peano | L DB | A DB DB | Free String | IN Integer deriving Show

index x xs = lookup x $ zip xs $ iterate Su Z

deBruijn = go [] where
  go binds = \case
    Var x -> maybe (Free x) N $ index x binds
    Lam x t -> L $ go (x:binds) t
    t `App` u -> A (go binds t) (go binds u)
    Int i -> IN i


convert :: ((Int, CL) -> (Int, CL) -> CL) -> DB -> (Int, CL)
convert (#) = \case
  N Z -> (1, Com I)
  N (Su e) -> (n + 1, (0, Com K) # t) where t@(n, _) = rec $ N e
  L e -> case rec e of
    (0, d) -> (0, Com K :@ d)
    (n, d) -> (n - 1, d)
  A e1 e2 -> (max n1 n2, t1 # t2) where
    t1@(n1, _) = rec e1
    t2@(n2, _) = rec e2
  Free s -> (0, Com (fromString s))
  IN i -> (0, INT i)
  where rec = convert (#)

plain :: DB -> (Int, CL)
plain = convert (#) where
  (0 , d1) # (0 , d2) = d1 :@ d2
  (0 , d1) # (n , d2) = (0, Com B :@ d1) # (n - 1, d2)
  (n , d1) # (0 , d2) = (0, Com R :@ d2) # (n - 1, d1)
  (n1, d1) # (n2, d2) = (n1 - 1, (0, Com S) # (n1 - 1, d1)) # (n2 - 1, d2)

bulkPlain :: (String -> Int -> CL) -> DB -> (Int, CL)
bulkPlain bulk = convert (#) where
  (a, x) # (b, y) = case (a, b) of
    (0, 0)             ->               x :@ y
    (0, n)             -> bulk "B" n :@ x :@ y
    (n, 0)             -> bulk "C" n :@ x :@ y
    (n, m) | n == m    -> bulk "S" n :@ x :@ y
           | n < m     ->                      bulk "B" (m - n) :@ (bulk "S" n :@ x) :@ y
           | otherwise -> bulk "C" (n - m) :@ (bulk "B" (n - m) :@  bulk "S" m :@ x) :@ y

bulk :: String -> Int -> CL
bulk c 1 = Com (fromString c)
bulk c n = Com (fromString (c ++ show n))


compileKiEither :: Environment -> (DB -> (Int, CL)) -> Either String (Int, CL)
compileKiEither env convertFun = case lookup "main" env of
  Nothing ->   Left $ error "main function missing in " ++ show env
  Just main -> Right $ convertFun $ deBruijn main


compileKi :: Environment -> (DB -> (Int, CL)) -> CL
compileKi env abstractFun =
  case compileKiEither env abstractFun of
    Left err     -> error $ show err
    Right (_,cl) -> cl


-- bulkOpt bulk = \case
--   N Z -> (True:[], Com "I")
--   N (Su e) -> head (False:) $ rec $ N e
--   L e -> case rec e of
--     ([], d) -> ([], Com "K" :@ d)
--     (False:g, d) -> ([], Com "K") ## (g, d)
--     (True:g, d) -> (g, d)
--   A e1 e2 -> rec e1 ## rec e2
--   Free s -> ([], Com s)
--   where
--   rec = bulkOpt bulk
--   ([], d1) ## ([], d2) = ([], d1 :@ d2)
--   ([], d1) ## ([True], Com "I") = ([True], d1)
--   ([], d1) ## (g2, Com "I") | and g2 = (g2, bulk "B" (length g2 - 1) :@ d1)
--   ([], d1) ## (g2@(h:_), d2) = first (pre++) $ ([], fun1 d1) ## (post, d2)
--     where
--     fun1 = case h of
--       True -> (bulk "B" (length pre) :@)
--       False -> id
--     (pre, post) = span (h ==) g2

--   ([True], Com "I") ## ([], d2) = ([True], Com "T" :@ d2)
--   (g1@(h:_), d1) ## ([], d2) = first (pre++) $ case h of
--     True -> ([], Com "C" :@ bulk "C" (length pre) :@ d2) ## (post, d1)
--     False -> (post, d1) ## ([], d2)
--     where
--     (pre, post) = span (h ==) g1

--   ([True], Com "I") ## (False:g2, d2) = first (True:) $ ([], Com "T") ## (g2, d2)
--   (False:g1, d1) ## ([True], Com "I") = (True:g1, d1)
--   (g1, d1) ## (g2, Com "I") | and g2, let n = length g2, all not $ take n g1 =
--     first (g2++) $ ([], bulk "B" $ n - 1) ## (drop n g1, d1)
--   (g1, d1) ## (g2, d2) = pre $ fun1 (drop count g1, d1) ## (drop count g2, d2)
--     where
--     (h, count) = headGroup $ zip g1 g2
--     fun1 = case h of
--       (False, False) -> id
--       (False, True) -> apply "B"
--       (True, False) -> apply "C"
--       (True, True) -> apply "S"
--     pre = first (replicate count (uncurry (||) h) ++)
--     apply s = (([], bulk s count) ##)

headGroup (h:t) = (h, 1 + length (takeWhile (== h) t))
