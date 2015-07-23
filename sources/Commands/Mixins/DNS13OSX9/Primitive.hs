{-# LANGUAGE DataKinds, NamedFieldPuns, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell                                            #-}
module Commands.Mixins.DNS13OSX9.Primitive where
import           Commands.Backends.OSX.Types     hiding (Command)
import           Commands.Core
import           Commands.Frontends.Dragon13
import           Commands.Mixins.DNS13OSX9.Types

import           Control.Lens
import           Data.Bifunctor
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NonEmpty
import qualified Data.Text.Lazy                  as T

import           Control.Exception               (SomeException (..))
import           Data.Char
import           Data.Foldable                   (asum)
import           Data.Proxy
import           Data.Tree
import           Data.Typeable
import           GHC.Exts                        (IsString (..))
import           Language.Haskell.TH.Syntax      (Name)


infix 1 <%>
infix 2 <=>

(<=>) :: Name -> H z a -> R z a
name <=> r = genericGrammar l r
 where
 Just l = lhsFromName name

(<%>) :: R z a -> (a -> Application -> Actions_) -> C z ApplicationDesugarer Actions_ a
(<%>) rule f = Command rule (ApplicationDesugarer f)


interpret :: (forall z. C z ApplicationDesugarer Actions_ a) -> Application -> String -> Either SomeException Actions_
-- TODO EitherT?
interpret c x s = case (c^.comRule) `parses` s of
 Left  e -> Left e
 Right a -> Right $ (c `compiles` a) x

-- | given a user's grammar, optimize it, validate it, serialize it.
--
-- 'DNSImport's all 'DNSBuiltinRules', whether used or not.
--
-- = INTERNALS
--
-- this function doesn't simply project the 'Rule'
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
serialized :: R z x -> Either [SomeException] SerializedGrammar
serialized rule = do
 let uG = DNSGrammar (getDescendentProductions rule) [] dnsHeader
 let oG = first T.pack (optimizeGrammar uG)
 vG <- escapeDNSGrammar oG
 return$ serializeGrammar vG
 -- TODO let grammar store multiple productions, or productions and vocabularies, or a whole grammar, even though the grammar doesn't need to store its transitive dependencies.

-- | a production and its transitive dependencies. removes duplicates. doesn't detect cycles.
getDescendentProductions :: (Eq l) => Rule p DNSReifying l i x -> NonEmpty (DNSReifyingProduction l i)
getDescendentProductions rule = NonEmpty.nubBy equalDNSProduction (export :| nonExports)
 where
 export = (rule^.ruleDNSProduction)
 nonExports = flatten =<< (rule^.ruleDNSDescendents)

parses :: (forall z. R z a) -> String -> Possibly a
parses rule s = NonEmpty.head <$> runRuleParser rule (words s)

compiles :: C z ApplicationDesugarer Actions_ a -> a -> Application -> Actions_
(c `compiles` a) x = runApplicationDesugarer (c^.comDesugar) a x
-- c `compiles` a `with` x
-- compiles :: Command a -> a -> Actions ()
-- c `compiles` a = (c^.comCompiler) a globalContext


-- |
--
-- the 'RHS' is "erased" into a grammar and a parser.
genericGrammar :: LHS -> H z a -> R z a
genericGrammar lhs rhs = Rule lhs g p
 where
 -- g = bimap T.pack T.pack $ renderRHS r
 g = induceDNSReified       lhs rhs
 p = induceEarleyProduction lhs rhs

-- | manually construct a special rule.
--
-- warning: partial function:
--
-- * match fails on non-global 'Name's
specialGrammar :: Name -> DNSReifying LHS String -> P z a
 -> Rule (EarleyProduction z LHS) DNSReifying LHS String a
 -- the z's must be shared, hence no R alias
specialGrammar name g p = Rule l g p
 where
 Just l = lhsFromName name      -- failed pattern match error messages contain a source location

-- | automatically generated a grammar from a type: the left-hand side comes from the type, and the right-hand side comes from the 'Show'n and transformed 'constructors'.
transformedGrammar :: forall z a. (Typeable a, Enum a, Show a) => (String -> String) -> R z a
transformedGrammar f = genericGrammar
 (lhsOfType (Proxy :: Proxy a))
 (asum . fmap (transformedCon f) $ constructors)

-- | helper function for conveniently using Dragon NaturallySpeaking built-ins.
dragonGrammar :: Name -> DNSReifyingRHS LHS String -> P z a
 -> Rule (EarleyProduction z LHS) DNSReifying LHS String a
dragonGrammar name r p = Rule
 l
 (DNSReifying $ Node (set (dnsProductionInfo.dnsInline) True (defaultDNSProduction l r)) [])
 p
 where
 Just l = lhsFromName name      -- failed pattern match error messages contain a source location

-- | a default 'Grammar' for 'Enum's.
--
-- with 'Enum's, we can get the "edit only once" property: edit the @data@ definition, then 'terminal' builds the 'Rule', and then the functions on 'Rule's build the 'Parser's and 'DNSGrammar's. with 'Typeable', but without TemplateHaskell.
--
-- the 'LHS' comes from the type, not the term (avoiding TemplateHaskell). other 'Grammar's can always be defined with an LHS that comes from the term, e.g. with '<=>' (as Haskell values' names are disjoint from Haskell types').
--
--
enumGrammar :: (Typeable a, Enum a, Show a) => R z a
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
qualifiedGrammar :: forall z a. (Typeable a, Enum a, Show a) => R z a
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
qualifiedGrammarWith :: (Typeable a, Enum a, Show a) => String -> R z a
qualifiedGrammarWith affix = transformedGrammar (overCamelCase (filter (/= fmap toLower affix)))

-- | strips out data typename like 'qualifiedGrammar', and @_@'s, and numbers.
-- makes it easy to generate generic terminals (like @"left"@),
-- without conflicting with.common symbols (like 'Left').
tidyGrammar :: forall z a. (Typeable a, Enum a, Show a) => R z a
tidyGrammar = transformedGrammar (overCamelCase (filter (/= fmap toLower occ)) . filter (/= '_'))
 where
 GUI _ _ (Identifier occ) = guiOf (Proxy :: Proxy a)

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
vocabularyGrammar :: (IsString a, Functor (p String)) => [String] -> RHS p r l String a
vocabularyGrammar = fmap fromString . vocabulary

-- | the empty grammar. See 'UnitDNSRHS' (always matches, recognizing nothing) and 'unitEarleyParser' (always succeeds, parsing nothing).
epsilon :: R z ()
epsilon = Rule l g p
 where
 l = unsafeLHSFromName 'epsilon
 g = defaultDNSReifying l UnitDNSRHS
 p = unitEarleyParser
