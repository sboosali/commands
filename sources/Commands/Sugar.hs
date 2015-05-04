{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances                                   #-}
module Commands.Sugar
 ( module Commands.Sugar
 , module Commands.Sugar.Press
 ) where
import Commands.Backends.OSX.Types         hiding (Command)
import Commands.Grammar
import Commands.Grammar.Types
import Commands.Mixins.DNS13OSX9.Primitive
import Commands.Mixins.DNS13OSX9.Types
import Commands.Sugar.Press

import Control.Lens                        (view)

import Language.Haskell.TH.Syntax          (Name)


infix  1 <%>
infix  2 <=>
-- infixl 3 <|>
-- infixl 4 <$>
-- infixl 4 <*>
infixl 4 # -- TODO   `(#) = review` or something in lens
infixl 4 & -- TODO   `(&) = flip ($)` in base in 7.10


(<=>) :: Name -> R a -> G a
name <=> r = genericGrammar l r
 where
 Just l = lhsFromName name

-- (<%>) :: Grammar p r a -> d a -> Command p r d a
-- (<%>) = Command
(<%>) :: G a -> (a -> Application -> Actions ()) -> C a
(<%>) g = Command g . ApplicationDesugarer


-- | like '<$' or '<$>', given the type (see the 'AppRHS' instances in this module).
--
-- e.g. inference for @True # "true"@ (__without__ @OverloadedStrings@):
--
-- @
-- (#) :: (AppRHS p r a) => LeftR a b -> a -> RHS b
-- -- given string literal ("true" :: String)
-- a ~ String
-- (#) :: (AppRHS String) => LeftR String b -> String -> RHS b
-- -- accept constraint 'AppRHS' and expand type family 'LeftR'
-- (#) :: b -> String -> RHS b
-- @
--
(#) :: (Functor p, AppRHS p r a) => LeftR a b -> a -> RHS p r b
f # x = pure f `appR` x

-- | like '<*' or '<*>', given the type (see the 'AppRHS' instances in this
-- module).
--
-- e.g. inference for @TODO@ (__without__ @OverloadedStrings@):
--
-- @
(&) :: (Functor p, AppRHS p r a) => RHS p r (LeftR a b) -> a -> RHS p r b
(&) = appR

-- | specialized 'appR' has types:
--
-- * @a        -> String    -> RHS a@
-- * @(a -> b) -> Grammar p r a -> RHS b@
-- * @(a -> b) -> RHS a     -> RHS b@
-- * etc.
--
class (ToRHS p r a) => AppRHS p r a where
 type LeftR a b :: *
 appR :: RHS p r (LeftR a b) -> a -> RHS p r b

instance (Functor p) => AppRHS p r String            where
 type LeftR String b              = b
 appR f x = f <*  toR x
instance (Functor p) => AppRHS p r (Command p r d a) where
 type LeftR (Command p r d a) b   = (a -> b)
 appR f x = f <*> toR x
instance (Functor p) => AppRHS p r (Grammar p r a)   where
 type LeftR (Grammar p r a) b     = (a -> b)
 appR f x = f <*> toR x
instance (Functor p) => AppRHS p r (RHS p r a)       where
 type LeftR (RHS p r a) b         = (a -> b)
 appR f x = f <*>     x
-- instance AppRHS p r [a]       where
--  type LeftR [a] b         = (a -> b)
--  appR f x = f <*> toR  x

-- | inject @a@s of different types into an @RHS@.
--
-- the first parameters (i.e. @p@ and @r@) are always abstract;
-- they seem to be needed to be in scope to unify with themselves in @a@.
class ToRHS p r a where
 type ToR a :: *
 toR :: a -> RHS p r (ToR a)

instance ToRHS p r String            where  type ToR String            = String;  toR = term
instance ToRHS p r (RHS     p r a)   where  type ToR (RHS     p r a)   = a;       toR = id
instance ToRHS p r (Grammar p r a)   where  type ToR (Grammar p r a)   = a;       toR = nont
instance ToRHS p r (Command p r d a) where  type ToR (Command p r d a) = a;       toR = nont . view comGrammar
-- instance (ToRHS p r a) => ToRHS p r [a]  where  type ToR [a] = a;       toR = foldMap toR

