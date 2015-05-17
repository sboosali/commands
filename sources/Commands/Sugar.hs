{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, TypeSynonymInstances                         #-}
module Commands.Sugar -- TODO rename to EBNF
 ( module Commands.Sugar
 , module Commands.Sugar.Press
 ) where
import Commands.Sugar.Press
import Commands.Symbol.Types

import Control.Lens          (view)


-- infixl 3 <|>
-- infixl 4 <$>
-- infixl 4 <*>
infixl 4 # -- TODO   `(#) = review` or something in lens
infixl 4 & -- TODO   `(&) = flip ($)` in base in 7.10


-- | like '<$' or '<$>', given the type (see the 'AppRHS' instances in this module).
--
-- e.g. inference for @True # "true"@ (__without__ @OverloadedStrings@):
--
-- @
-- (#) :: (AppRHS p r l i a) => LeftR a b -> a -> RHS b
-- -- given string literal ("true" :: String)
-- a ~ String
-- (#) :: (AppRHS String) => LeftR String b -> String -> RHS b
-- -- accept constraint 'AppRHS' and expand type family 'LeftR'
-- (#) :: b -> String -> RHS b
-- @
--
(#) :: (Functor (p i), AppRHS p r l i a) => LeftR a b -> a -> RHS p r l i b
f # x = pure f `appR` x

-- | like '<*' or '<*>', given the type (see the 'AppRHS' instances in this
-- module).
--
-- e.g. inference for @TODO@ (__without__ @OverloadedStrings@):
--
-- @
(&) :: (Functor (p i), AppRHS p r l i a) => RHS p r l i (LeftR a b) -> a -> RHS p r l i b
(&) = appR

-- | specialized 'appR' has types:
--
-- * @a        -> String    -> RHS a@
-- * @(a -> b) -> Rule p r a -> RHS b@
-- * @(a -> b) -> RHS a     -> RHS b@
-- * etc.
--
class (ToRHS p r l i a) => AppRHS p r l i a where
 type LeftR a b :: *
 appR :: RHS p r l i (LeftR a b) -> a -> RHS p r l i b

instance (Functor (p String)) => AppRHS p r l String String            where
 type LeftR String b              = b
 appR f x = f <*  toR x
instance ((p1 ~ p2), Functor (p2 i)) => AppRHS p1 r l i (Command p2 r d l i b a) where
 type LeftR (Command p r d l i b a) a'   = (a -> a')
 appR f x = f <*> toR x
instance ((p1 ~ p2), Functor (p2 i)) => AppRHS p1 r l i (Rule p2 r l i a) where
 -- the equality constraint delays unification until after the instance head is committed to,
 -- e.g. (p2 ~ EarleyProduction z) needs this; the z's universally quantified and unconstrained, and don't unify with each other when an RHS is defined.
 type LeftR (Rule p r l i a) b     = (a -> b)
 appR f x = f <*> toR x
instance ((p1 ~ p2), Functor (p2 i)) => AppRHS p1 r l i (RHS p2 r l i a) where
 type LeftR (RHS p r l i a) b         = (a -> b)
 appR f x = f <*>     x
-- instance AppRHS p r l i [a]       where
--  type LeftR [a] b         = (a -> b)
--  appR f x = f <*> toR  x

-- | inject @a@s of different types into an @RHS@.
--
-- the first parameters (i.e. @p@ and @r@) are always abstract;
-- they seem to be needed to be in scope to unify with themselves in @a@.
class ToRHS p r l i a where
 type ToR a :: *
 toR :: a -> RHS p r l i (ToR a)

instance              ToRHS p  r l String String              where
  type ToR String                   = String;
  toR = word
instance (p1 ~ p2) => ToRHS p1 r l i (RHS     p2 r   l i   a) where
  type ToR (RHS     p2 r   l i   a) = a;
  toR = id
instance (p1 ~ p2) => ToRHS p1 r l i (Rule    p2 r   l i   a) where
  type ToR (Rule    p2 r   l i   a) = a;
  toR = rule
instance (p1 ~ p2) => ToRHS p1 r l i (Command p2 r d l i b a) where
  type ToR (Command p2 r d l i b a) = a;
  toR = rule . view comRule
-- instance (ToRHS p r l i a) => ToRHS p r l i [a]  where  type ToR [a] = a;       toR = foldMap toR
-- instance (IsString i) => ToRHS p r l i String            where  type ToR String            = String;  toR = word . fromString

