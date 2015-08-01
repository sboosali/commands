{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, LambdaCase #-}
{-# LANGUAGE PostfixOperators, ScopedTypeVariables, TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Commands.Plugins.Example.Phrase where
import           Commands.Core
import           Commands.Frontends.Dragon13
import           Commands.Mixins.DNS13OSX9
import           Commands.Plugins.Example.Spacing
import           Data.Sexp

import           Control.Lens                hiding (from, (#))
import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Semigroup

import           Control.Applicative
import           Data.Char
import           Data.Foldable               (Foldable (..))
import qualified Data.List                   as List
import           Data.Typeable               (Typeable)
import           GHC.Exts                    (IsString (..))
import           Prelude                     hiding (foldr1, mapM)


-- ================================================================ --

-- | user-facing Phrase. exists at (DSL) parse-time.
type Phrase  = PhraseF (Either Pasted PAtom)
-- | "Mungeable Phrase". exists only at (DSL) run-time.
-- the atom is really a list of atoms, but not a full Sexp. this supports "splatting". We interpret a list of atoms as string of words with white space between, more or less.
type MPhrase = PhraseF [PAtom]
type PhraseF = Sexp PFunc
asPhrase :: String -> Phrase
asPhrase = Atom . Right . PWord

-- | a "Phrase Function".
data PFunc
 = Cased      Casing
 | Joined     Joiner
 | Surrounded Brackets
 deriving (Show,Eq,Ord)

data Casing = Upper | Lower | Capper deriving (Show,Eq,Ord,Enum,Typeable)
data Joiner = Joiner String | CamelJoiner | ClassJoiner deriving (Show,Eq,Ord)
data Brackets = Brackets String String deriving (Show,Eq,Ord)
-- not {JoinerFunction ([String] -> String)} to keep the {Eq} instance for caching
-- fake equality? {JoinerFunction Name ([String] -> String)} so {JoinerFunction 'camelCase camelCase}
-- maybe some type from data.split package, that both supports decidable equality and that can build functions
bracket :: Char -> Brackets
bracket c = Brackets [c] [c]

-- | "Phrase Atom".
--
-- 'PAcronym's behave differently from 'PWord's under some 'Joiner's (e.g. class case).
-- a 'PAcronym' should hold only uppercase letters.
data PAtom
 = PWord String
 | PAcronym [Char]
 deriving (Show,Eq,Ord)

-- | for doctest
instance IsString PAtom where fromString = PWord

{- | a Pasted is like a 'Dictation'/'Quoted_' i.e. a list of words, not a single word.

we know (what words are in) the Dictation at (DSL-)"parse-time" i.e. 'phrase', 
but we only know (what words are in) the Pasted at (DSL-)"runtime" 
(i.e. wrt the DSL, not Haskell). Thus, it's a placeholder.


-}
data Pasted = Pasted  deriving (Show,Eq,Ord)

{- | 'Phrase_' versus 'Phrase':

@Phrase_@ is the unassociated concrete syntax list (e.g. tokens, parentheses),
while @Phrase@ is the associated abstract syntax tree (e.g. s-expressions).

-}
data Phrase_
 = Escaped_  Keyword -- ^ atom-like (wrt 'Sexp').
 | Quoted_   Dictation -- ^ list-like.
 | Pasted_ -- ^ atom-like.
 | Blank_ -- ^ atom-like.
 | Separated_ Separator -- ^ like a "close paren".
 | Cased_      Casing -- ^ function-like (/ "open paren").
 | Joined_     Joiner -- ^ function-like (/ "open paren").
 | Surrounded_ Brackets -- ^ function-like (/ "open paren").
 | Capped_   [Char] -- ^ atom-like.
 | Spelled_  [Char] -- ^ list-like.
 | Dictated_ Dictation -- ^ list-like.
 deriving (Show,Eq,Ord)

-- | used by 'pPhrase'.
--
--
type PStack = NonEmpty PItem
-- -- the Left represents 'List', the Right represents 'Sexp', 'Atom' is not represented.
-- type PStack = NonEmpty (Either [Phrase] (PFunc, [Phrase]))

-- | an inlined subset of 'Sexp'.
--
-- Nothing represents 'List', Just represents 'Sexp', 'Atom' is not represented.
type PItem = (Maybe PFunc, [Phrase])



-- ================================================================ --

-- | splats the Pasted into PAtom's, after splitting the clipboard into words
splatPasted :: Phrase -> String -> MPhrase
splatPasted p clipboard = either (substPasted clipboard) (:[]) <$> p
 where
 substPasted pasted Pasted = fmap PWord (words pasted)

mungePhrase :: MPhrase -> Spaced String
mungePhrase p = concatPAtoms =<< evalSplatSexp applyPFunc p

{- |


>>> :set -XOverloadedStrings
>>> concatPAtoms ["",""]
PWord ""
>>> concatPAtoms ["",""]
PWord ""

-}
concatPAtoms :: [PAtom] -> Spaced String
concatPAtoms xs e = (flip spaceOut) e . fmap mungePAtom $ xs

mungePAtom :: PAtom -> String
mungePAtom = \case
 PWord    x  -> x
 PAcronym cs -> upper cs

applyPFunc :: [PAtom] -> PFunc -> Spaced [PAtom]
applyPFunc as = \case
  Cased      g -> traverse (return . caseWith g) as
  Joined     g -> return [joinWith g as]
  Surrounded g -> surroundWith g as

caseWith :: Casing -> (PAtom -> PAtom)
caseWith casing = mapPAtom (fromCasing casing)

fromCasing :: Casing -> (String -> String)
fromCasing = \case
 Upper  -> upper
 Lower  -> lower
 Capper -> capitalize

mapPAtom :: (String -> String) -> (PAtom -> PAtom)
mapPAtom f = \case
 PWord    x  -> PWord    $ f x
 PAcronym cs -> PAcronym $ f cs
 -- PWord . f . mungePAtom

joinWith :: Joiner -> ([PAtom] -> PAtom)
joinWith = \case
 -- Joiner s    -> List.interleave (PWord s)
 Joiner s    -> PWord . List.intercalate s . fmap mungePAtom
 CamelJoiner -> PWord . camelAtoms
 ClassJoiner -> PWord . classAtoms

camelAtoms :: [PAtom] -> String
camelAtoms []     = ""
camelAtoms (x:xs) = lower (mungePAtom x) <> (classAtoms xs)

classAtoms :: [PAtom] -> String
classAtoms = squeezeCase . (fmap $ \case
 PWord w     -> capitalize w
 PAcronym cs -> upper cs)
-- TODO distinguish Capped from Acronym to preserve capitalization?

surroundWith :: Brackets -> ([PAtom] -> Spaced [PAtom])
surroundWith (Brackets l r) as = do
 -- xs <- traverse mungePAtom as
 return $ ([PWord l] <> as <> [PWord r])
-- TODO generalize by renaming surround to transform: it shares the type with Interleave
-- e.g. "par thread comma 123" -> (1,2,3)

joinSpelled :: [Phrase_] -> [Phrase_]
joinSpelled = foldr' go []
 where
 go :: Phrase_ -> [Phrase_] -> [Phrase_]
 go (Spelled_ xs) (Spelled_ ys : ps) = (Spelled_ $ xs <> ys) : ps
 go p ps = p:ps

-- | parses "tokens" into an "s-expression". a total function.
pPhrase :: [Phrase_] -> Phrase
pPhrase = fromStack . foldl' go ((Nothing, []) :| []) . joinSpelled
 -- (PSexp (PList [PAtom (PWord "")]))
 where
 go :: PStack -> Phrase_ -> PStack
 go ps = \case
  (Escaped_  (x))            -> update ps $ fromPAtom (PWord x)
  (Quoted_   (Dictation xs)) -> update ps $ List ((fromPAtom . PWord) <$> xs)
  (Dictated_ (Dictation xs)) -> update ps $ List ((fromPAtom . PWord) <$> xs)
  (Capped_   cs)             -> update ps $ fromPAtom (PAcronym cs)
  (Spelled_  cs)             -> update ps $ fromPAtom (PAcronym cs)
  Pasted_                    -> update ps $ fromPasted
  Blank_                     -> update ps $ fromPAtom (PWord "")
  Separated_ (Separator x) -> update (pop ps) $ fromPAtom (PWord x)
  -- Separated_ Broken -> update (pop ps)
  (Cased_     f)  -> push ps (Cased f)
  (Joined_    f)  -> push ps (Joined f)
  (Surrounded_ f) -> push ps (Surrounded f)

 pop :: PStack -> PStack
 -- break from the innermost PFunc, it becomes an argument to the outer PFunc
 -- i.e. close the S expression with a right parenthesis "...)"
 pop ((Nothing,ps):|(q:qs)) = update (q:|qs) (List ps)
 pop ((Just f ,ps):|(q:qs)) = update (q:|qs) (Sexp f ps)
 -- if too many breaks, just ignore
 pop stack = stack
 -- i.e. open a left parenthesis with some function "(f ..."
 push :: PStack -> PFunc -> PStack
 push (p:|ps) f = (Just f, []) :| (p:ps)

 update :: PStack -> Phrase -> PStack
 update ((f,ps):|qs) p = (f, ps <> [p]) :| qs

 -- right-associate the PFunc's.
 fromStack :: PStack -> Phrase
 fromStack = fromItem . foldr1 associateItem . NonEmpty.reverse

 associateItem :: PItem -> PItem -> PItem
 associateItem (f,ps) = \case
  (Nothing,qs) -> (f, ps <> [List   qs])
  (Just g ,qs) -> (f, ps <> [Sexp g qs])

 fromItem :: PItem -> Phrase
 fromItem (Nothing, ps) = List   ps
 fromItem (Just f,  ps) = Sexp f ps

 fromPasted :: Phrase
 fromPasted = Atom . Left $ Pasted

 fromPAtom :: PAtom -> Phrase
 fromPAtom = Atom . Right


-- ================================================================ --

-- |
--
-- transforms "token"s from 'phrase_' into an "s-expression" with 'pPhrase'.
phrase = pPhrase <$> phrase_

-- |
--
phrase_ :: R z [Phrase_]
phrase_ = 'phrase <=>
 (\ps p -> ps <> [p]) <#> ((phraseA -|- phraseB -|- phraseW)-*) # (phraseB -|- phraseC -|- phraseD)

 -- = Rule
 -- (unsafeLHSFromName 'phrase)
 -- (induceDNSReified (((phraseA -|- phraseB -|- phraseD)-*) # (phraseB -|- phraseC -|- phraseD)))
 -- (induceEarleyProduction)

-- | a sub-phrase where a phrase to the right is certain.
--
-- this ordering prioritizes the escaping Escaped_/Quoted_ over the
-- escaped, e.g. "quote greater equal unquote".
phraseA :: R z Phrase_
phraseA = 'phraseA <=> empty
 <|> Escaped_    <#> "lit" # keyword
 <|> Quoted_     <#> "quote" # dictation # "unquote"
 <|> Pasted_     <#> "paste"
 <|> Blank_      <#> "blank"
 <|> (Spelled_) <#> letter_
 <|> (Spelled_ . (:[])) <#> character
 <|> Separated_  <#> separator
 <|> Cased_      <#> casing
 <|> Joined_     <#> joiner
 <|> Surrounded_ <#> brackets

-- | a sub-phrase where a phrase to the right is possible.
phraseB :: R z Phrase_
phraseB = 'phraseB <=> empty
 -- TODO letters grammar that consumes tokens with multiple capital letters, as well as tokens with single aliases
 -- <|> Spelled_  <#> "spell" # letters -- only, not characters
 <|> Spelled_  <#> "spell" # (character-+)
 <|> Capped_   <#> "caps" # (character-+)
 -- <$> alphabetRHS

-- | a sub-phrase where a phrase to the right is impossible.
phraseC :: R z Phrase_
phraseC = 'phraseC <=> Dictated_ <#> "say" # dictation

-- | injects dictation into phrase_
phraseW :: R z Phrase_
phraseW = 'phraseD <=> (Dictated_ . Dictation . (:[])) <#> word_

-- | injects dictation into phrase_
phraseD :: R z Phrase_
phraseD = 'phraseD <=> Dictated_ <#> dictation

type Keyword = String -- TODO
keyword :: R z Keyword
keyword = 'keyword <=> id<#>word_

newtype Separator = Separator String  deriving (Show,Eq,Ord)
separator = 'separator <=> empty
 <|> Separator ""  <#> "break" -- separation should depend on context i.e. blank between symbols, a space between words, space after a comma but not before it. i.e. the choice is delayed until munging.
 <|> Separator " " <#> "space"
 <|> Separator "," <#> "comma"

casing = enumGrammar

joiner = 'joiner
 <=> (\c -> Joiner [c]) <#> "join" # character
 <|> Joiner "_" <#> "snake"
 <|> Joiner "-" <#> "dash"
 <|> Joiner "/" <#> "file"
 <|> Joiner ""  <#> "squeeze"
 <|> CamelJoiner <#> "camel"
 <|> ClassJoiner <#> "class"

brackets :: R z Brackets
brackets = 'brackets
 <=> bracket          <#> "round" # character
 <|> Brackets "(" ")" <#> "par"
 <|> Brackets "[" "]" <#> "square"
 <|> Brackets "{" "}" <#> "curl"
 <|> Brackets "<" ">" <#> "angle"
 <|> bracket '"'      <#> "string"
 <|> bracket '\''     <#> "ticked"
 <|> bracket '|'      <#> "norm"
 -- <|> Brackets "**" "**" <#> "bold"

character = 'character <=> empty

 <|> '`' <#> "grave"
 <|> '~' <#> "till"
 <|> '!' <#> "bang"
 <|> '@' <#> "axe"
 <|> '#' <#> "pound"
 <|> '$' <#> "doll"
 <|> '%' <#> "purse"
 <|> '^' <#> "care"
 <|> '&' <#> "amp"
 <|> '*' <#> "star"
 <|> '(' <#> "lore"
 <|> ')' <#> "roar"
 <|> '-' <#> "dash"
 <|> '_' <#> "score"
 <|> '=' <#> "eek"
 <|> '+' <#> "plus"
 <|> '[' <#> "lack"
 <|> '{' <#> "lace"
 <|> ']' <#> "rack"
 <|> '}' <#> "race"
 <|> '\\' <#> "stroke"
 <|> '|' <#> "pipe"
 <|> ';' <#> "sem"
 <|> ':' <#> "coal"
 <|> '\'' <#> "tick"
 <|> '"' <#> "quote"
 <|> ',' <#> "com"
 <|> '<' <#> "less"
 <|> '.' <#> "dot"
 <|> '>' <#> "great"
 <|> '/' <#> "slash"
 <|> '?' <#> "quest"
 <|> ' ' <#> "ace"
 <|> '\t' <#> "tab"
 <|> '\n' <#> "line"

 <|> '0' <#> "zero"
 <|> '1' <#> "one"
 <|> '2' <#> "two"
 <|> '3' <#> "three"
 <|> '4' <#> "four"
 <|> '5' <#> "five"
 <|> '6' <#> "six"
 <|> '7' <#> "seven"
 <|> '8' <#> "eight"
 <|> '9' <#> "nine"

 <|> 'a' <#> "ay"
 <|> 'b' <#> "bee"
 <|> 'c' <#> "sea"
 <|> 'd' <#> "dee"
 <|> 'e' <#> "eek"
 <|> 'f' <#> "eff"
 <|> 'g' <#> "gee"
 <|> 'h' <#> "aych"
 <|> 'i' <#> "eye"
 <|> 'j' <#> "jay"
 <|> 'k' <#> "kay"
 <|> 'l' <#> "el"
 <|> 'm' <#> "em"
 <|> 'n' <#> "en"
 <|> 'o' <#> "oh"
 <|> 'p' <#> "pea"
 <|> 'q' <#> "queue"
 <|> 'r' <#> "are"
 <|> 's' <#> "ess"
 <|> 't' <#> "tea"
 <|> 'u' <#> "you"
 <|> 'v' <#> "vee"
 <|> 'w' <#> "dub"
 <|> 'x' <#> "ex"
 <|> 'y' <#> "why"
 <|> 'z' <#> "zee"
 <|> alphabetRHS


{- | equivalent to:

@
 <|> 'a' <#> "A"
 <|> 'b' <#> "B"
 <|> 'c' <#> "C"
 <|> ...
 <|> 'z' <#> "Z"
@

-}
-- alphabetRHS :: Functor (p i) => RHS p r l String Char
alphabetRHS = foldMap (\c -> c <$ word [toUpper c]) ['a'..'z']
-- TODO What will we get back from Dragon anyway?


newtype Dictation = Dictation [String] deriving (Show,Eq,Ord)
dictation = dragonGrammar 'dictation
 (DNSNonTerminal (SomeDNSLHS (DNSBuiltinRule DGNDictation)))
 (EarleyProduction $ Dictation <$> some anyToken)

word_ = dragonGrammar 'word_
 (DNSNonTerminal (SomeDNSLHS (DNSBuiltinRule DGNWords)))
 (EarleyProduction $ anyToken)

letter_ = dragonGrammar 'letter_
 (DNSNonTerminal (SomeDNSLHS (DNSBuiltinRule DGNLetters)))
 (EarleyProduction $ anyLetter)

-- newtype Letters = Letters [Char] deriving (Show,Eq,Ord)
-- letters = (set dnsInline True defaultDNSInfo) $ 'letters <=>
--  Letters <#> (letter-+)
 -- TODO greedy (many) versus non-greedy (manyUntil)
