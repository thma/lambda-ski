{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoImplicitPrelude  #-}

{-- This module exposes a function simplify which takes a FreeCat expression as input and
    returns an equivalent yet syntactically simplified FreeCat expression.

    > toCCC @FreeCat (\(x, y) -> x)
    Comp Fst Id

    > simplify $ toCCC (\(x, y) -> x)
    Fst
--}

module CCC.Rewrite (simplify) where

import           CCC.Cat
import           CCC.FreeCat
import           Prelude hiding (id, (.))

type Rule = forall a b. FreeCat a b -> Maybe (FreeCat a b)

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
ruleParDupEq :: Rule -- FreeCat a b -> Maybe (FreeCat a b)
ruleParDupEq (Comp (Par f g) Dup) | f == g = Just (Dup . f)
ruleParDupEq _                             = Nothing
--}

-- build the curry rules.
ruleCurry :: Rule
ruleCurry (Curry (Uncurry f)) = Just f
ruleCurry _                   = Nothing

ruleCurry' :: Rule
ruleCurry' _ = Nothing

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
    ruleCurry',
    ruleCurryApply,
    ruleParen
  ]

-- Note: Rewrite rules for IfThenElse (if True => f, if False => g)
-- are complex due to GADT type constraints. These could be added
-- later with appropriate type witnesses.

maxDepth :: Int
maxDepth = 1000

-- Avoid infinite loops by allowing only a recursion depth of `maxDepth`
recurseMatch :: Int -> Rule -> FreeCat a b -> Maybe (FreeCat a b)
recurseMatch 0 _rule _x = Nothing
recurseMatch depth rule x = case rule x of
  Nothing -> goDown (recurseMatch (depth -1) rule) x -- This rule didn't match. Try going down and matching there.
  Just x' -> Just x'

goDown :: Rule -> Rule --FreeCat a b -> Maybe (FreeCat a b)
goDown z (Comp f g) = case z f of
  Nothing -> case z g of
    Nothing -> Nothing
    Just x  -> Just (Comp f x)
  Just x -> Just (Comp x g)
goDown z (Par f g) = case z f of
  Nothing -> case z g of
    Nothing -> Nothing -- nothing in either subtree macthed
    Just x  -> Just (Par f x) --
  Just x -> Just (Par x g) -- something in f matched the rule
goDown z (Curry f) = case z f of
  Nothing -> Nothing
  Just x  -> Just (Curry x)
goDown z (Uncurry f) = case z f of
  Nothing -> Nothing
  Just x  -> Just (Uncurry x)
goDown _ _ = Nothing -- can't go down

rewrite' :: [Rule] -> [Rule] -> FreeCat a b -> FreeCat a b
rewrite' _ [] k = k -- no rules matched
rewrite' allrules (rule : rules) k = case recurseMatch maxDepth rule k of
  Nothing -> rewrite' allrules rules k -- try the next rule
  Just k' -> rewrite' allrules allrules k' -- start over from the beginning

rewrite :: [Rule] -> FreeCat a b -> FreeCat a b
rewrite rules = rewrite' rules rules

simplify :: FreeCat a b -> FreeCat a b
simplify = rewrite allRules