{-# LANGUAGE DataKinds, NamedFieldPuns, RankNTypes, ScopedTypeVariables #-}
module Commands.Command where
import           Commands.Command.Types
import           Commands.Etc
import           Commands.Frontends.Dragon13
import           Commands.Frontends.Dragon13.Render
import           Commands.Grammar
import           Commands.Grammar.Types
import           Commands.Parse
import           Commands.Parse.Types
-- import           Commands.Frontends.Dragon13.Text
import           Commands.Frontends.Dragon13.Types
import           Control.Alternative.Free.Tree

import           Control.Applicative
import           Control.Monad.Catch                (SomeException (..))
import           Data.Bifunctor                     (bimap)
import           Data.Foldable                      (asum)
import           Data.Proxy
import qualified Data.Text.Lazy                     as T
import           Data.Typeable                      (Typeable)
import           Language.Haskell.TH.Syntax         (Name)


infix  2 <=>
-- infixl 3 <|>
-- infixl 4 <$>
infixl 4 #
-- infixl 4 <*>
infixl 9 &


(<=>) :: Name -> RHS a -> Command a
name <=> r = genericCommand l r
 where
 Just l = fromName name

-- | a default 'Command' for simple ADTs.
--
-- with 'Enum' ADTs, we can get the "edit only once" property: edit the @data@ definition, then 'terminal' builds the 'Rule', and then the functions on 'Rule's build the 'Parser's and 'DNSGrammar's. without TemplateHaskell.
--
-- the 'LHS' comes from the type, not the term (avoiding TemplateHaskell). other 'Command's can always be defined with an LHS that comes from the term, e.g. with '<=>'.
--
--
defaultCommand :: forall a. (Typeable a, Enum a, Show a) => Command a
defaultCommand = genericCommand
 (guiOf (Proxy :: Proxy a))
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
 Just l = fromName name

-- (&) :: (Grammatical a) => RHSs (a -> b) -> a -> RHSs b
-- f & x = f <*> toR x
(&) :: Applicative f => f (a -> b) -> f a -> f b
f & x = f <*> x

-- (#) :: (Grammatical a) => (a -> b) -> a -> RHSs b
-- f # x = f <$> toR x
(#) :: Functor f => (a -> b) -> f a -> f b
f # x = f <$> x

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

project :: Command a -> RHS a
project = lift . fromCommand

serialized :: Command x -> Either [SomeException] T.Text
serialized Command{_grammar} = serialize $ bimap T.pack T.pack $ _grammar

parses :: Command a -> String -> Possibly a
parses Command{_parser} = parsing _parser

