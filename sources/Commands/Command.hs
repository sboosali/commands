{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
module Commands.Command where
import           Commands.Etc
import Commands.Command.Types
import Commands.Grammar
import Commands.Grammar.Types
import Commands.Parse
import Commands.Parse.Types
-- import Commands.Frontends.Dragon13
import Commands.Frontends.Dragon13.Types
import Commands.Frontends.Dragon13.Text
import Commands.Frontends.Dragon13.Render
import           Control.Alternative.Free.Tree

import           Data.Proxy
import           Language.Haskell.TH.Syntax    (Name)
import           Data.Foldable                 (asum)
import           Data.Typeable                 (Typeable)
import Control.Applicative
import           Data.Bifunctor                    (bimap)
import qualified Data.Text.Lazy                    as T

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
genericCommand l r = Command rule g p
 where
 g = bimap T.pack T.pack  $ renderProduction rule -- TODO 
 p = gparser rule
 rule = Rule l r

-- | builds a special 'Command' directly, not indirectly via 'Rule'.
specialCommand :: Name -> DNSProduction True Text Text -> Parser a -> Command a
specialCommand name g p = Command rule g p
 where
 rule = Rule l empty
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
project (Command rule _ _) = lift . fromRule $ rule

-- -- |
-- renders :: Command x -> DNSGrammar Text Text
-- renders = bimap T.pack T.pack . render

