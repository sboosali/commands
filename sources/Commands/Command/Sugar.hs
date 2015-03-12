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
infixl 4 #
infixl 4 <&>
infixl 4 <&
infixl 4 &>
infixl 4 &


(<=>) :: Name -> RHS a -> Command a
name <=> r = genericCommand l r
 where
 l = unsafeLHSFromName name



-- | e.g. inference for @True # "true"@ (__no__ @OverloadedStrings@):
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

-- | e.g. inference for @TODO@ (__no__ @OverloadedStrings@):
--
-- @
(&) :: (AppRHS a) => RHS (LeftR a b) -> a -> RHS b
(&) = appR

-- | specialized 'appR' has types:
--
-- * @a        -> String    -> RHS a@
-- * @(a -> b) -> Command a -> RHS b@
-- * @(a -> b) -> RHS a     -> RHS b@
--
--
class (ToRHS a) => AppRHS a where
 type LeftR a b :: *
 appR  :: RHS (LeftR a b) -> a -> RHS b

instance AppRHS String      where
 type LeftR String b      = b
 appR f x = f <*  toR x
instance AppRHS (Command a) where
 type LeftR (Command a) b = (a -> b)
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
instance ToRHS (Command a) where  type ToR (Command a) = a;       toR = liftCommand
instance ToRHS (RHS a)     where  type ToR (RHS a)     = a;       toR = id

