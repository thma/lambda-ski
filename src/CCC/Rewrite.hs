{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoImplicitPrelude  #-}

{-- | Simplification/normalization of categorical expressions (CatExpr).
    Applies syntactic rewrite rules to produce equivalent simplified expressions.

    > simplify (toCCC @CatExpr (\(x, y) -> x))
    Fst
--}

module CCC.Rewrite (simplify) where

import           CCC.Cat
import           CCC.CatExpr
import           Prelude hiding (id, (.))
import           Debug.Trace (trace)

type Rule = forall a b. CatExpr a b -> Maybe (CatExpr a b)

ruleParen :: Rule
ruleParen (Comp (Comp f g) h) = Just (Comp f (Comp g h))
ruleParen _                   = Nothing

ruleFstsndpar :: Rule
ruleFstsndpar (Comp (Par Fst Snd) Dup) = Just Id
ruleFstsndpar _                        = Nothing

ruleFstDup :: Rule
ruleFstDup (Comp Fst Dup) = Just Id
ruleFstDup _              = Nothing

ruleSndDup :: Rule
ruleSndDup (Comp Snd Dup) = Just Id
ruleSndDup _              = Nothing

ruleParDup :: Rule
ruleParDup (Comp (Par (Comp f Fst) (Comp g Snd)) Dup) = Just (Par f g)
ruleParDup _                                          = Nothing

ruleParDup' :: Rule
ruleParDup' (Comp (Par (Comp f Fst) (Comp g Fst)) Dup) = Just ((Par f g . Dup) . Fst)
ruleParDup' _ = Nothing

ruleParDup'' :: Rule
ruleParDup'' (Comp (Par (Comp f Snd) (Comp g Snd)) Dup) = Just ((Par f g . Dup) . Snd)
ruleParDup'' _ = Nothing

-- parC dupC" forall f. (_parC f f) . _dupC = _dupC . f
{- -- needs equality.
ruleParDupEq :: Rule -- CatExpr a b -> Maybe (CatExpr a b)
ruleParDupEq (Comp (Par f g) Dup) | f == g = Just (Dup . f)
ruleParDupEq _                             = Nothing
--}

ruleCurry :: Rule
ruleCurry (Curry (Uncurry f)) = Just f
ruleCurry _                   = Nothing

ruleCurryApply :: Rule
ruleCurryApply (Curry Apply) = Just Id
ruleCurryApply _             = Nothing

ruleIdLeft :: Rule
ruleIdLeft (Comp Id f) = Just f
ruleIdLeft _           = Nothing

ruleIdRight :: Rule
ruleIdRight (Comp f Id) = Just f
ruleIdRight _           = Nothing

allRules :: [Rule]
allRules =
  [ ruleFstsndpar,
    ruleIdRight,
    ruleIdLeft,
    ruleFstDup,
    ruleSndDup,
    ruleParDup,
    ruleParDup',
    ruleParDup'',
    ruleCurry,
    ruleCurryApply,
    ruleParen
  ]

-- Note: Scott-encoded booleans (Fst/Snd as selectors) benefit from
-- existing Fst/Snd rewrite rules without needing special boolean rules.

maxDepth :: Int
maxDepth = 1000

-- Avoid infinite loops by allowing only recursion depth of `maxDepth`
recurseMatch :: Int -> Rule -> CatExpr a b -> Maybe (CatExpr a b)
recurseMatch 0 _rule _x = Nothing
recurseMatch depth rule x = case rule x of
  Nothing -> goDown (recurseMatch (depth -1) rule) x
  Just x' ->
    trace ("Rule applied: " ++ show rule ++ " to " ++ show x ++ " => " ++ show x') $ Just x'

goDown :: Rule -> Rule
goDown z (Comp f g) = case z f of
  Nothing -> case z g of
    Nothing -> Nothing
    Just x  -> Just (Comp f x)
  Just x -> Just (Comp x g)
goDown z (Par f g) = case z f of
  Nothing -> case z g of
    Nothing -> Nothing
    Just x  -> Just (Par f x)
  Just x -> Just (Par x g)
goDown z (Curry f) = case z f of
  Nothing -> Nothing
  Just x  -> Just (Curry x)
goDown z (Uncurry f) = case z f of
  Nothing -> Nothing
  Just x  -> Just (Uncurry x)
goDown _ _ = Nothing

rewrite' :: [Rule] -> [Rule] -> CatExpr a b -> CatExpr a b
rewrite' _ [] k = k
rewrite' allrules (rule : rules) k = case recurseMatch maxDepth rule k of
  Nothing -> rewrite' allrules rules k
  Just k' -> rewrite' allrules allrules k'

rewrite :: [Rule] -> CatExpr a b -> CatExpr a b
rewrite rules = rewrite' rules rules

simplify :: CatExpr a b -> CatExpr a b
simplify = rewrite allRules