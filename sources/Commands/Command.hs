{-# LANGUAGE DataKinds, FlexibleInstances, NamedFieldPuns, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeFamilies       #-}
{-# LANGUAGE TypeSynonymInstances                                     #-}
module Commands.Command where
import           Commands.Command.Types             ()
import           Commands.Etc
import           Commands.Frontends.Dragon13
import           Commands.Frontends.Dragon13.Render
import           Commands.Grammar
import           Commands.Grammar.Types
import           Commands.Parse
import           Commands.Parse.Types
-- import           Commands.Frontends.Dragon13.Text
import           Commands.Frontends.Dragon13.Types
import           Commands.Parsec
-- import           Control.Alternative.Free.Tree

import           Control.Applicative
import           Control.Applicative.Permutation
import           Control.Monad.Catch                (SomeException (..),
                                                     catches)
import           Data.Bifunctor                     (bimap)
import           Data.Foldable                      (asum)
import           Data.Proxy
import qualified Data.Text.Lazy                     as T
import           Data.Typeable                      (Typeable)
import           Language.Haskell.TH.Syntax         (Name)
import qualified Text.Parsec                        as Parsec


infix  2 <=>
-- infixl 3 <|>
infixl 4 #
-- infixl 4 <$>
-- infixl 4 <*>
infixl 4 <&>
infixl 4 <&
infixl 4 &>
-- infixl 9 &


(<&>) :: (R f,  R x,  ToR f ~ (a -> b),  ToR x ~ a)  =>  f -> x -> RHS b
f <&> x = toR f <*> toR x

(<&) :: (R x,  R y)  =>  x -> y -> RHS (ToR x)
x <& y = toR x <* toR y

(&>) :: (R x,  R y)  =>  x -> y -> RHS (ToR y)
x &> y = toR x *> toR y

class    R a           where  type ToR a           :: *;      toR :: a -> RHS (ToR a)
instance R String      where  type ToR String      = String;  toR = liftString
instance R (Command a) where  type ToR (Command a) = a;       toR = liftCommand
instance R (RHS a)     where  type ToR (RHS a)     = a;       toR = id

(#) :: Applicative f => (a -> b) -> f a -> f b
(#) = (<$>)

-- (&) :: Applicative f => f (a -> b) -> f a -> f b
-- f & x = f <*> x

-- | specialized 'appR' has types:
--
-- * @a        -> String    -> RHS a@
-- * @(a -> b) -> Command a -> RHS b@
-- * @(a -> b) -> RHS a     -> RHS b@
--
--
-- class    (R a) => R0 a  where  type AppR a           :: *;      appR :: AppR b a
-- instance R0 String      where  type AppR String    = a -> String -> RHS a;  appR f x = f <$  toR x
-- instance R0 (Command a) where  type AppR (Command a) = (a -> b) -> Command a -> RHS b;       appR f x = f <$> toR x
-- instance R0 (RHS a)     where  type AppR (RHS a)     = (a -> b) -> RHS a     -> RHS b;       appR f x = f <$>     x

-- class Command a b c | a b -> c where
-- instance Command String  String
-- instance Command Command Command
-- instance Command String  Command
-- instance Command Command Command

-- class Grammatical a where
--  type R a :: *
--  toR :: a -> RHSs (R a)

-- instance Grammatical (RHSs b)    where  type R (RHSs b)    = b;  toR = id
-- instance Grammatical String      where  type R String      = b;  toR = lift . Terminal
-- -- instance Grammatical (Rule b) where  type R (Rule b) = b;  toR = lift
-- instance Grammatical (Rule b) where
--  type R (Rule b) = b
--  toR (Terminal s)      = toR s
--  toR (NonTerminal _ r) = r



serialized :: Command x -> Either [SomeException] T.Text
serialized Command{_grammar} = serialize $ bimap T.pack T.pack $ _grammar

parses :: Command a -> String -> Possibly a
parses Command{_parser} = parsing _parser

handleParse :: Show a => Command a -> String -> IO ()
handleParse command s = do
 (print =<< (command `parses` s)) `catches` parseHandlers
 putStrLn ""



(<=>) :: Name -> RHS a -> Command a
name <=> r = genericCommand l r
 where
 l = unsafeLHSFromName name

-- | a default 'Command' for simple ADTs.
--
-- with 'Enum' ADTs, we can get the "edit only once" property: edit the @data@ definition, then 'terminal' builds the 'Rule', and then the functions on 'Rule's build the 'Parser's and 'DNSGrammar's. without TemplateHaskell.
--
-- the 'LHS' comes from the type, not the term (avoiding TemplateHaskell). other 'Command's can always be defined with an LHS that comes from the term, e.g. with '<=>'.
--
--
defaultCommand :: forall a. (Typeable a, Enum a, Show a) => Command a
defaultCommand = genericCommand
 (LHS . guiOf $ (Proxy :: Proxy a))
 (asum . fmap con $ constructors)

-- |
genericCommand :: LHS -> RHS a -> Command a
genericCommand l r = Command l g p
 where
 -- g = bimap T.pack T.pack $ renderRHS r
 g = renderRule (Rule l r)
 p = rparser r

-- | builds a special 'Command' directly, not indirectly via 'Rule'.
specialCommand :: Name -> DNSGrammar String String -> Parser a -> Command a
specialCommand name g p = Command l g p
 where
 l = unsafeLHSFromName name



multipleC :: Command a -> Command [a]
multipleC Command{_lhs,_grammar,_parser} = Command
 lhs
 (multipleDNSGrammar (showLHS lhs) _grammar)
 -- (\context -> _parser (Some parserUnit) `manyUntil` context) -- assumes _parser is context free
 (\context -> _parser context `manyUntil` context)
 where
 lhs = unsafeLHSFromName 'multipleC `LHSApp` [_lhs]

multipleDNSGrammar :: String -> DNSGrammar String t -> DNSGrammar String t
multipleDNSGrammar name (DNSGrammar production productions) = DNSGrammar
 (DNSProduction (DNSRule name) (hoistDNSRHS DNSMultiple production))
 (upcastDNSProduction production : productions)

rhsMaybe :: RHS a -> Perms RHS (Maybe a)
rhsMaybe = maybeAtom

commandMaybe :: Command a -> Perms RHS (Maybe a)
commandMaybe = atom . liftCommand . optionalC

optionC :: a -> Command a -> Command a
optionC theDefault Command{_lhs,_grammar,_parser} = Command
 lhs
 (optionalDNSGrammar (showLHS lhs) _grammar)
 (\context -> Parsec.option theDefault $ _parser context)
 where
 lhs = unsafeLHSFromName 'optionC `LHSApp` [_lhs]

optionalC :: Command a -> Command (Maybe a)
optionalC Command{_lhs,_grammar,_parser} = Command
 lhs
 (optionalDNSGrammar (showLHS lhs) _grammar)
 (\context -> Parsec.optionMaybe $ _parser context)
 where
 lhs = unsafeLHSFromName 'optionalC `LHSApp` [_lhs]

optionalDNSGrammar :: String -> DNSGrammar String t -> DNSGrammar String t
optionalDNSGrammar name (DNSGrammar production productions) = DNSGrammar
 (DNSProduction (DNSRule name) (hoistDNSRHS DNSOptional production))
 (upcastDNSProduction production : productions)
