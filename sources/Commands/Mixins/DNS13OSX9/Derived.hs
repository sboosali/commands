{-# LANGUAGE TemplateHaskell, RankNTypes, ScopedTypeVariables   #-}
module Commands.Mixins.DNS13OSX9.Derived where 
import           Commands.Extra
import           Commands.Munging
import Commands.Mixins.DNS13OSX9.Types 
import Commands.Frontends.Dragon13
import Commands.RHS.Types 
import Commands.Parsers.Earley

import qualified Text.Earley                     as E
import Control.Lens hiding (snoc) 

import Data.Void
import           Data.Char
import           Data.Proxy
import           Data.Typeable
import GHC.Exts (IsString(..))
import           Language.Haskell.TH.Syntax      (Name)
import qualified Data.List as List
import Data.Foldable (asum) 


infix 2 <=>

{-| @(<=>) = 'genericGrammar'@ 

-}
(<=>) :: Name -> DNSEarleyRHS z a -> DNSEarleyRHS z a
(<=>) = genericGrammar
-- NOTE specialized type signature is for type inference, disambiguates:
--  "No instance for (Data.String.IsString _)" and "No instance for (Functor _)"

{-| construct a 'NonTerminal'.

-}
nonterminalGrammar :: String -> DNSEarleyRHS z a -> DNSEarleyRHS z a
nonterminalGrammar l r = NonTerminal (ConstName (defaultDNSInfo, l)) r

{-| construct a 'NonTerminal'.

-}
genericGrammar :: Name -> DNSEarleyRHS z a -> DNSEarleyRHS z a
genericGrammar name r = nonterminalGrammar (gui^.(guiIdentifier._Identifier)) r
 where
 Just gui = fromGlobalName name  -- TODO GHC 7.10.2 https://downloads.haskell.org/~ghc/7.10.2/docs/html/users_guide/other-type-extensions.html#special-implicit-params

{-| manually construct a special rule, with primitives. 

-}
simpleGrammar :: Name -> (E.Prod z String Text a) -> (DNSRHS Text Void) -> DNSEarleyRHS z a
simpleGrammar n p r = genericGrammar n $ liftLeaf p r

{-| manually construct a special rule, with separate recognizer and parser. 

it has two independent right-hand sides: one for recognizing and one for parsing.

-}
complexGrammar :: Name -> DNSEarleyRHS z a -> DNSEarleyRHS z a -> DNSEarleyRHS z a
complexGrammar n p r = genericGrammar n $ liftTree p r

{-| automatically generate a grammar from a type.

the left-hand side comes from the type, 
and the right-hand side comes from the 'Show'n and transformed 'constructors'. 

-}
transformedGrammar :: forall z a. (Typeable a, Enum a, Show a) => (String -> String) -> DNSEarleyRHS z a
transformedGrammar f = nonterminalGrammar
 (guiOf(Proxy :: Proxy a) ^. (guiIdentifier._Identifier))  -- TODO Haskell type sections, whenever
 (asum . fmap (transformedCon f) $ constructors)

{-| helper function for conveniently using Dragon NaturallySpeaking built-ins. 

sets 'dnsInline' to true. 

-}
dragonGrammar :: Name -> (E.Prod z String Text a) -> DNSBuiltinRule -> DNSEarleyRHS z a
dragonGrammar name p r = simpleGrammar name p (SomeDNSNonTerminal (DNSBuiltinRule r))
 & set (_RHSInfo.dnsInline) True

{-| a default 'Grammar' for 'Enum's.

with 'Enum's, we can get the "edit only once" property: edit the @data@ definition, then 'terminal' builds the 'Rule', and then the functions on 'Rule's build the 'Parser's and 'DNSGrammar's. with 'Typeable', but without TemplateHaskell.

the 'LHS' comes from the type, not the term (avoiding TemplateHaskell). other 'Grammar's can always be defined with an LHS that comes from the term, e.g. with '<=>' (as Haskell values' names are disjoint from Haskell types').

-}
enumGrammar :: (Typeable a, Enum a, Show a) => DNSEarleyRHS z a
enumGrammar = transformedGrammar (overCamelCase id)

{-| a default 'Grammar' for simple ADTs.

detects the type name in a constructor name (as a
prefix/infix/suffix) and elides the affix.

useful when you want your @grammars@-DSL terminals to be
unqualified (for convenience), but you want your Haskell
identifiers to be qualified (to avoid conflicts). e.g.:

e.g. avoids naming conflicts with @Either@:

>>> :set -XDeriveDataTypeable
>>> data Button = LeftButton | ButtonMiddleButton | ButtonRight deriving (Show,Eq,Enum,Typeable)
>>> let button = qualifiedGrammar :: Grammar Button
>>> getWords . view gramGrammar $ button
["left","middle","right"]

(the qualification is exaggerated to show the filtering behavior:
it's consistent in idiomatic declarations).

we didn't define @data Button = Left | Middle | Right@ because it
conflicts with 'Either', but the derived grammar is identical.

-}
qualifiedGrammar :: forall z a. (Typeable a, Enum a, Show a) => DNSEarleyRHS z a
qualifiedGrammar = qualifiedGrammarWith occ
 where
 occ = guiOf(Proxy :: Proxy a) ^. (guiIdentifier._Identifier)

{-| a default 'Grammar' for simple ADTs.

elides the given <http://en.wikipedia.org/wiki/Affix affix> from any part of any constructor.

e.g. avoids naming conflicts with @Either@. without making either the data type name too short, or the data constructor names too long:

>>> :set -XDeriveDataTypeable
>>> data Direction = UpD | DownD | LeftD | RightD  deriving (Show,Eq,Enum,Typeable)
>>> qualifiedGrammarWith "D" :: Grammar Direction
["up","down","left","right"]

-}
qualifiedGrammarWith :: (Typeable a, Enum a, Show a) => String -> DNSEarleyRHS z a
qualifiedGrammarWith affix = transformedGrammar (overCamelCase (filter (/= fmap toLower affix)))

{-| strips out data typename like 'qualifiedGrammar', along with @_@'s and numbers.

makes it easy to generate generic terminals (like @"left"@),
without conflicting with.common symbols (like 'Left').

-}
tidyGrammar :: forall z a. (Typeable a, Enum a, Show a) => DNSEarleyRHS z a
tidyGrammar = transformedGrammar f 
 where
 f = filter (/= '_') >>> overCamelCase (filter (/= fmap toLower occ))
 occ = guiOf(Proxy :: Proxy a) ^. (guiIdentifier._Identifier)

{-| a default 'Grammar' for 'String' @newtype@s.

the user might want to parse/recognize an arbitrary but dynamic/large subset of all possible strings.
For example:

* a mutable grammer whose contents depend on some context,
like the current buffer, or the previous recognition.
* a huge list of custom words, that sound like more common words,
that aren't being recognized, even after using Dragon's Vocabulary Builder.
* even a few static words, which don't need to be a sum typo,
to not increase boilerplate, while still increasing type safety.

e.g.

@
newtype Place = Place String deriving (Show,Eq)
instance IsString Place where fromString = Place
@

-}
vocabularyGrammar :: [String] -> DNSEarleyRHS z Text
vocabularyGrammar = tokens

{-| the empty grammar. 

See 'UnitDNSRHS' (which always matches, recognizing nothing) 
and 'unitEarleyParser' (which always succeeds, parsing nothing). 

-}
epsilon :: DNSEarleyRHS z ()
epsilon = simpleGrammar 'epsilon unitEarleyParser UnitDNSRHS

--TODO generalize these introducers to any RHS, and use Text

{-| inject a string.

-}
token :: (IsString t, Show t) => String -> RHS n t f t
token = fromString

{-| @str = 'token'@

specialized for type inference. 
(with @OverloadedStrings@, string literals are 'IsString'-constrained polymorphic types)
 
-}
str :: String -> DNSEarleyRHS z Text
str = token

{-| inject a character. 

-}
chr :: Char -> DNSEarleyRHS z Char
chr c = c <$ token [c]

{-| @int = 'con'@

specialized for type inference. 
(integer literals are 'Num'-constrained polymorphic types).

-}
int :: Int -> DNSEarleyRHS z Int
int = con

{-| inject a constructor. 

-}
con :: (Show a) => a -> DNSEarleyRHS z a
con = transformedCon (List.intercalate " " . unCamelCase)

{-| make a 'Terminal' from the @transformed@ 'Show'n constructor, returning the constructor. 

-}
transformedCon :: (Show a) => (String -> String) -> a -> DNSEarleyRHS z a
transformedCon f x = x <$ (token . f . show $ x)

-- | @= 'optionRHS' 'enumDefault' ...@
optionalEnum :: (Enum a) => DNSEarleyRHS z a -> DNSEarleyRHS z a
optionalEnum = optionRHS enumDefault

{-| inject a mapping.

in each pair, the first is the string to recognize, the second is the value to parse and return. 

-}
vocab :: (IsString t, Show t, Functor'RHS n t f) => [(String, a)] -> RHS n t f a
vocab
 = foldMap (\(s,x) -> x <$ token s)
 . filterBlanks

{-| inject a set of strings.  

-}
tokens :: (IsString t, Show t, Functor'RHS n t f) => [String] -> RHS n t f t
tokens = foldMap token

