{-# LANGUAGE DataKinds, NamedFieldPuns, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell                                            #-}
module Commands.Command where
import           Commands.Etc
import           Commands.Frontends.Dragon13
import           Commands.Frontends.Dragon13.Optimize
import           Commands.Frontends.Dragon13.Render
import           Commands.Frontends.Dragon13.Types
import           Commands.Grammar
import           Commands.Grammar.Types
import           Commands.Parse
import           Commands.Parse.Types
import           Commands.Parsec                      (parserUnit)

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Catch                  (SomeException (..),
                                                       catches)
import           Data.Bifunctor                       (second)
import           Data.Foldable                        (asum)
import qualified Data.List                            as List
import           Data.List.NonEmpty                   (NonEmpty (..))
import qualified Data.Map                             as Map
import           Data.Proxy
import qualified Data.Text.Lazy                       as T
import           Data.Typeable                        (Typeable)
import           GHC.Generics                         (Generic)
import           Language.Haskell.TH.Syntax           (Name)


-- |
--
-- 'DNSImport's all 'DNSBuiltinRules', whether used or not.
--
-- = INTERNALS
--
-- this function doesn't simply project the 'Command'
-- onto its 'DNSProduction' with '_comGrammar',
-- the way 'parses' simply extracts what it needs with '_comParser'.
-- instead, this function expects the '_comRule' to be correct, from
-- which it extracts the descendents the root production depends on.
--
-- one problem is that this implicit dependency between '_comGrammar'
-- and '_comRule' forces library authors to maintain consistency
-- between '_comGrammar' and '_comRule' within every 'Command'.
--
-- however,
-- this shouldn't be a problem for library users, given that
-- consistency is guaranteed by the library author.
-- this is because the "higher level" 'genericCommand' API derives
-- '_comGrammar' from '_comRule'.
-- if a library user uses the "lower-level" 'specialCommand', then
-- they will need __want__ to learn some internals anyway.
--
-- a previous alternative to had been for '_comGrammar' to be a 'Lens'
-- onto a 'DNSGrammar', not a 'DNSProduction'. naïvely storing your
-- descendents in a linear structure (i.e. the list of 'DNSProduction'
-- in a 'DNSGrammar') caused non-termination when serializing
-- mutually-recursive 'Command's.
--
-- a possible future alternative might
-- be for a production to store it's transitive productions
-- intelligently. But then, why not reuse the power of 'RHS'?
--
-- an even more complex future alternative might be to represent
-- the DNSGrammar with higher-order syntax.
-- then, the mutually recursive references recursion would be direct
-- (not indirect as now), just like Parsec.
-- if that makes things better and not worse, that would be cool.
--
serialized :: Command x -> Either [SomeException] T.Text
serialized command = serialize . second T.pack . optimizeGrammar $ DNSGrammar
  (getDescendentProductions command)
  []
  dnsHeader
 -- TODO let command store multiple productions, or productions and vocabularies, or a whole grammar, even though the grammar doesn't need to store its transitive dependencies.

-- | the transitive dependencies of a grammar. doesn't double count the 'dnsExport' when it's its own descendent.
getDescendentProductions :: Command x -> NonEmpty DNSCommandProduction
getDescendentProductions command = export :| List.delete export productions
 where
 export = command^.comGrammar
 productions = ((\(Some command) -> command^.comGrammar) <$> (Map.elems . reifyCommand $ command))
-- TODO store productions, grammars, commands?

-- -- | the non-transitive dependencies of a grammar.
-- getChildrenProductions :: Command x -> NonEmpty DNSCommandProduction
-- getChildrenProductions = view (comGrammar.dnsProductions)


parses :: Command a -> String -> Possibly a
parses command = parsing (command ^. comParser)

handleParse :: Show a => Command a -> String -> IO ()
handleParse command s = do
 (print =<< (command `parses` s)) `catches` parseHandlers


-- |
genericCommand :: LHS -> RHS a -> Command a
genericCommand l r = Command (Rule l r) g p
 where
 -- g = bimap T.pack T.pack $ renderRHS r
 g = renderRule (Rule l r)
 p = rparser r

-- | builds a special 'Command' directly, not indirectly via 'Rule'.
--
-- warning: partial function:
--
-- * match fails on non-global 'Name's
specialCommand :: Name -> RHS a -> DNSCommandGrammar -> Parser a -> Command a
specialCommand name r g p = Command (Rule l r) g p
 where
 Just l = lhsFromName name

-- | helper function for conveniently defined Dragon NaturallySpeaking built-ins.
dragonCommand :: Name -> DNSCommandRHS -> Parser a -> Command a
dragonCommand name rhs p = Command
 (Rule l empty)
 (DNSProduction (set dnsInline True defaultDNSInfo) (DNSRule (defaultDNSExpandedName l)) rhs)
 p
 where
 Just l = lhsFromName name

-- | a default 'Command' for simple ADTs.
--
-- with 'Enum' ADTs, we can get the "edit only once" property: edit the @data@ definition, then 'terminal' builds the 'Rule', and then the functions on 'Rule's build the 'Parser's and 'DNSGrammar's. without TemplateHaskell.
--
-- the 'LHS' comes from the type, not the term (avoiding TemplateHaskell). other 'Command's can always be defined with an LHS that comes from the term, e.g. with '<=>'.
--
--
enumCommand :: forall a. (Typeable a, Enum a, Show a) => Command a
enumCommand = genericCommand
 (lhsOfType (Proxy :: Proxy a))
 (asum . fmap con $ constructors)

-- | a default 'Command' for simple ADTs.
--
-- detects the type name in a constructor name (as a
-- prefix/infix/suffix) and elides the affix.
--
-- useful when you want your @commands@-DSL terminals to be
-- unqualified (for convenience), but you want your Haskell
-- identifiers to be qualified (to avoid conflicts). e.g.:
--
-- >>> :set -XDeriveDataTypeable
-- >>> data Button = LeftButton | ButtonMiddleButton | ButtonRight deriving (Show,Eq,Enum,Typeable)
-- >>> let button = qualifiedCommand :: Command Button
-- >>> getWords . view comGrammar $ button
-- ["left","middle","right"]
--
-- (the qualification is exaggerated to show the filtering behavior:
-- it's consistent in idiomatic declarations).
--
-- we didn't define @data Button = Left | Middle | Right@ because it
-- conflicts with 'Either', but the derived grammar is identical.
--
--
--
--
qualifiedCommand :: forall a. (Typeable a, Enum a, Show a) => Command a
qualifiedCommand = genericCommand
 (lhsOfType (Proxy :: Proxy a))
 (asum . fmap (qualifiedCon occ) $ constructors)
 where
 GUI _ _ (Identifier occ) = guiOf (Proxy :: Proxy a)

-- | a default 'Command' for 'String' @newtype@s.
--
-- e.g. @newtype Place = Place String deriving (Show,Eq)@
--
vocabularyCommand :: (Generic a) => [String] -> RHS a
vocabularyCommand = undefined vocabulary
-- (Newtype a String) => [String] -> RHS a

-- | the empty grammar. See 'unitDNSRHS'.
epsilon :: Command ()
epsilon = Command (Rule l r) g p
 where
 Just l = lhsFromName 'epsilon
 r = empty
 g = DNSProduction defaultDNSInfo (DNSRule (defaultDNSExpandedName l)) unitDNSRHS  -- TODO def method
 p = \_ -> parserUnit
