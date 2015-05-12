{-# LANGUAGE DataKinds, NamedFieldPuns, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell                                            #-}
module Commands.Mixins.DNS13OSX9.Primitive where
import           Commands.Backends.OSX.Types
import           Commands.Etc
import           Commands.Frontends.Dragon13.Optimize
import           Commands.Frontends.Dragon13.Render
import           Commands.Frontends.Dragon13.Serialize
import           Commands.Frontends.Dragon13.Types
import           Commands.Grammar
import           Commands.Grammar.Types
import           Commands.Mixins.DNS13.Types
import           Commands.Mixins.DNS13OSX9.Types
import           Commands.Munging
import           Commands.Parse
import           Commands.Parse.Types
import           Commands.Parsec                       (parserUnit)

import           Control.Lens
import           Data.Bifunctor                        (second)
import qualified Data.List                             as List
import           Data.List.NonEmpty                    (NonEmpty (..))
import qualified Data.Text.Lazy                        as T

import           Control.Applicative
import           Control.Exception                     (SomeException (..))
import           Data.Char
import           Data.Foldable                         (asum)
import qualified Data.Map                              as Map
import           Data.Proxy
import           Data.Typeable                         (Typeable)
import           GHC.Exts                              (IsString (..))
import           Language.Haskell.TH.Syntax            (Name)


interpret :: C a -> Application -> String -> Either SomeException (Actions ())
-- TODO EitherT?
interpret c x s = case (c^.comGrammar) `parses` s of
 Left  e -> Left e
 Right a -> Right $ (c `compiles` a) x

-- |
--
-- 'DNSImport's all 'DNSBuiltinRules', whether used or not.
--
-- = INTERNALS
--
-- this function doesn't simply project the 'Grammar'
-- onto its 'DNSProduction' with '_gramGrammar',
-- the way 'parses' simply extracts what it needs with '_gramParser'.
-- instead, this function expects the '_gramRule' to be correct, from
-- which it extracts the descendents the root production depends on.
--
-- one problem is that this implicit dependency between '_gramGrammar'
-- and '_gramRule' forces library authors to maintain consistency
-- between '_gramGrammar' and '_gramRule' within every 'Grammar'.
--
-- however,
-- this shouldn't be a problem for library users, given that
-- consistency is guaranteed by the library author.
-- this is because the "higher level" 'genericGrammar' API derives
-- '_gramGrammar' from '_gramRule'.
-- if a library user uses the "lower-level" 'specialGrammar', then
-- they will need __want__ to learn some internals anyway.
--
-- a previous alternative to had been for '_gramGrammar' to be a 'Lens'
-- onto a 'DNSGrammar', not a 'DNSProduction'. naïvely storing your
-- descendents in a linear structure (i.e. the list of 'DNSProduction'
-- in a 'DNSGrammar') caused non-termination when serializing
-- mutually-recursive 'Grammar's.
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
-- a Kleisli arrow.
--
serialized :: Grammar p DNSReifying x -> Either [SomeException] SerializedGrammar
serialized grammar = do
 let g = second T.pack . optimizeGrammar $ DNSGrammar (getDescendentProductions grammar) [] dnsHeader
 eg <- escapeDNSGrammar g
 return $ serializeGrammar eg
 -- TODO let grammar store multiple productions, or productions and vocabularies, or a whole grammar, even though the grammar doesn't need to store its transitive dependencies.

-- | the transitive dependencies of a grammar. doesn't double count the 'dnsExport' when it's its own descendent.
getDescendentProductions :: Grammar p DNSReifying x -> NonEmpty DNSReifyingProduction
getDescendentProductions grammar = export :| List.delete export productions
 where
 export = grammar^.gramGrammar
 productions = ((\(SomeGrammar grammar) -> grammar^.gramGrammar) <$> (Map.elems . reifyGrammar $ grammar))
-- TODO store productions, grammars, commands?

-- -- | the non-transitive dependencies of a grammar.
-- getChildrenProductions :: Grammar x -> NonEmpty DNSReifyingProduction
-- getChildrenProductions = view (gramGrammar.dnsProductions)


parses :: Grammar Parser r a -> String -> Possibly a
parses grammar = parsing (grammar ^. gramParser)

compiles :: Command p r ApplicationDesugarer a -> a -> Application -> Actions ()
(c `compiles` a) x = runApplicationDesugarer (c^.comCompiler) a x
-- c `compiles` a `with` x
-- compiles :: Command a -> a -> Actions ()
-- c `compiles` a = (c^.comCompiler) a globalContext

{- TODO open imports with RecordWildCards and data

how to handle Grammar versus Command?

maybe something like this (less ugly) :

data CommandI p i r s b a =
 { serialized :: Grammar p i r s b a   -> Possibly s
 , parses     :: Grammar p i r s b a   -> i -> Possibly a
 , compiles   :: Command p i r s d b a -> a -> b
 }

iEarleyDNS13OSX9 :: CommandI String EarleyParser DNSReifying SerializedGrammar ApplicationDesugarer
iEarleyDNS13OSX9 = CommandI{..}
 where
 serialized =
 parses =
 compiles =

import Commands.Mixins.DNS13OSX9 (iEarleyDNS13OSX9)
CommandI{..} = iEarleyDNS13OSX9

maybe make this whole dang module a pseudo-Module record. ugg

-}

-- |
--
-- the 'RHS' is "erased" into a grammar and a parser.
genericGrammar :: LHS -> R a -> G a
genericGrammar l r = Grammar (Rule l r) g p
 where
 -- g = bimap T.pack T.pack $ renderRHS r
 g = induceDNSProduction (Rule l r)
 p = rparser r

-- | builds a special 'Grammar' directly, not indirectly via 'Rule'.
--
-- warning: partial function:
--
-- * match fails on non-global 'Name's
specialGrammar :: Name -> R a -> DNSReifying -> Parser a -> G a
specialGrammar name r g p = Grammar (Rule l r) g p
 where
 Just l = lhsFromName name

-- | helper function for conveniently using Dragon NaturallySpeaking built-ins.
dragonGrammar :: Name -> DNSReifyingRHS -> Parser a -> G a
dragonGrammar name rhs p = Grammar
 (Rule l empty)
 (DNSProduction (set dnsInline True defaultDNSInfo) (DNSRule (defaultDNSExpandedName l)) rhs)
 p
 where
 Just l = lhsFromName name

-- | a default 'Grammar' for 'Enum's.
--
-- with 'Enum's, we can get the "edit only once" property: edit the @data@ definition, then 'terminal' builds the 'Rule', and then the functions on 'Rule's build the 'Parser's and 'DNSGrammar's. with 'Typeable', but without TemplateHaskell.
--
-- the 'LHS' comes from the type, not the term (avoiding TemplateHaskell). other 'Grammar's can always be defined with an LHS that comes from the term, e.g. with '<=>'.
--
--
enumGrammar :: (Typeable a, Enum a, Show a) => G a
enumGrammar = transformedGrammar (overCamelCase id)

-- | a default 'Grammar' for simple ADTs.
--
-- detects the type name in a constructor name (as a
-- prefix/infix/suffix) and elides the affix.
--
-- useful when you want your @grammars@-DSL terminals to be
-- unqualified (for convenience), but you want your Haskell
-- identifiers to be qualified (to avoid conflicts). e.g.:
--
-- e.g. avoids naming conflicts with @Either@:
--
-- >>> :set -XDeriveDataTypeable
-- >>> data Button = LeftButton | ButtonMiddleButton | ButtonRight deriving (Show,Eq,Enum,Typeable)
-- >>> let button = qualifiedGrammar :: Grammar Button
-- >>> getWords . view gramGrammar $ button
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
qualifiedGrammar :: forall a. (Typeable a, Enum a, Show a) => G a
qualifiedGrammar = qualifiedGrammarWith occ
 where
 GUI _ _ (Identifier occ) = guiOf (Proxy :: Proxy a)

-- | a default 'Grammar' for simple ADTs.
--
-- elides the given <http://en.wikipedia.org/wiki/Affix affix> from any part of any constructor.
--
-- e.g. avoids naming conflicts with @Either@. without making either the data type name too short, or the data constructor names too long:
--
-- >>> :set -XDeriveDataTypeable
-- >>> data Direction = UpD | DownD | LeftD | RightD  deriving (Show,Eq,Enum,Typeable)
-- >>> qualifiedGrammarWith "D" :: Grammar Direction
-- ["up","down","left","right"]
--
--
qualifiedGrammarWith :: (Typeable a, Enum a, Show a) => String -> G a
qualifiedGrammarWith affix = transformedGrammar (overCamelCase (filter (/= fmap toLower affix)))

-- | strips out data typename like 'qualifiedGrammar', and @_@'s, and numbers.
-- makes it easy to generate generic terminals (like @"left"@),
-- without conflicting with.common symbols (like 'Left').
tidyGrammar :: forall a. (Typeable a, Enum a, Show a) => G a
tidyGrammar = transformedGrammar (overCamelCase (filter (/= fmap toLower occ)) . filter (/= '_'))
 where
 GUI _ _ (Identifier occ) = guiOf (Proxy :: Proxy a)

-- | automatically generated a grammar from a type: the left-hand side comes from the type, and the right-hand side comes from the 'Show'n and transformed 'constructors'.
transformedGrammar :: forall a. (Typeable a, Enum a, Show a) => (String -> String) -> G a
transformedGrammar f = genericGrammar
 (lhsOfType (Proxy :: Proxy a))
 (asum . fmap (transformedCon f) $ constructors)

-- | a default 'Grammar' for 'String' @newtype@s.
--
-- the user might want to parse/recognize an arbitrary but dynamic/large subset of all possible strings.
-- For example:
--
-- * a mutable grammer whose contents depend on some context,
-- like the current buffer, or the previous recognition.
-- * a huge list of custom words, that sound like more common words,
-- that aren't being recognized, even after using Dragon's Vocabulary Builder.
-- * even a few static words, which don't need to be a sum typo,
-- to not increase boilerplate, while still increasing type safety.
--
-- e.g.
--
-- @
-- newtype Place = Place String deriving (Show,Eq)
-- instance IsString Place where fromString = Place
-- @
--
--
vocabularyGrammar :: (IsString a, Functor p) => [String] -> RHS p r a
vocabularyGrammar = fmap fromString . vocabulary

-- | the empty grammar. See 'UnitDNSRHS'.
epsilon :: G ()
epsilon = Grammar (Rule l r) g p
 where
 Just l = lhsFromName 'epsilon
 r = empty
 g = DNSProduction defaultDNSInfo (DNSRule (defaultDNSExpandedName l)) UnitDNSRHS  -- TODO def method
 p = freeParser parserUnit
