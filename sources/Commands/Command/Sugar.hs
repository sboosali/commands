{-# LANGUAGE FlexibleInstances, TypeFamilies, TypeSynonymInstances #-}
module Commands.Command.Sugar where
import Commands.Command
import Commands.Etc               ()
import Commands.Grammar
import Commands.Grammar.Types

import Control.Applicative
import Language.Haskell.TH.Syntax (Name)



infix  2 <=>
-- infixl 3 <|>
-- infixl 4 <$>
-- infixl 4 <*>
infixl 4 # -- TODO   `(#) = review` or something in lens
infixl 4 <&>
infixl 4 <&
infixl 4 &>
infixl 4 & -- TODO   `(&) = flip ($)` in base in 7.10


(<=>) :: Name -> RHS a -> Grammar a
name <=> r = genericGrammar l r
 where
 Just l = lhsFromName name



-- | like '<$' or '<$>', given the type (see the 'AppRHS' instances in this module).
--
-- e.g. inference for @True # "true"@ (__without__ @OverloadedStrings@):
--
-- @
-- (#) :: (AppRHS a) => LeftR a b -> a -> RHS b
-- -- given string literal ("true" :: String)
-- a ~ String
-- (#) :: (AppRHS String) => LeftR String b -> String -> RHS b
-- -- accept constraint 'AppRHS' and expand type family 'LeftR'
-- (#) :: b -> String -> RHS b
-- @
--
(#) :: (AppRHS a) => LeftR a b -> a -> RHS b
f # x = pure f `appR` x

-- | like '<*' or '<*>', given the type (see the 'AppRHS' instances in this
-- module).
--
-- e.g. inference for @TODO@ (__without__ @OverloadedStrings@):
--
-- @
(&) :: (AppRHS a) => RHS (LeftR a b) -> a -> RHS b
(&) = appR

-- | specialized 'appR' has types:
--
-- * @a        -> String    -> RHS a@
-- * @(a -> b) -> Grammar a -> RHS b@
-- * @(a -> b) -> RHS a     -> RHS b@
--
--
class (ToRHS a) => AppRHS a where
 type LeftR a b :: *
 appR  :: RHS (LeftR a b) -> a -> RHS b

instance AppRHS String      where
 type LeftR String b      = b
 appR f x = f <*  toR x
instance AppRHS (Grammar a) where
 type LeftR (Grammar a) b = (a -> b)
 appR f x = f <*> toR x
instance AppRHS (RHS a)     where
 type LeftR (RHS a) b     = (a -> b)
 appR f x = f <*>     x


(<&>) :: (ToRHS f,  ToRHS x,  ToR f ~ (a -> b),  ToR x ~ a)  =>  f -> x -> RHS b
f <&> x = toR f <*> toR x

(<&) :: (ToRHS x,  ToRHS y)  =>  x -> y -> RHS (ToR x)
x <& y = toR x <* toR y

(&>) :: (ToRHS x,  ToRHS y)  =>  x -> y -> RHS (ToR y)
x &> y = toR x *> toR y

class    ToRHS a           where  type ToR a           :: *;      toR :: a -> RHS (ToR a)
instance ToRHS String      where  type ToR String      = String;  toR = liftString
instance ToRHS (Grammar a) where  type ToR (Grammar a) = a;       toR = liftGrammar
instance ToRHS (RHS a)     where  type ToR (RHS a)     = a;       toR = id

