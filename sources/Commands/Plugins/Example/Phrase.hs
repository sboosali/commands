{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DeriveFunctor, LambdaCase, TypeFamilies          #-}
{-# LANGUAGE PostfixOperators, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Commands.Plugins.Example.Phrase where
import           Commands.Plugins.Example.Spacing

import qualified Commands.Backends.OSX            as OSX
import           Commands.Etc
import           Commands.Frontends.Dragon13
import           Commands.Mixins.DNS13OSX9
import           Commands.Munging
import           Data.Sexp

import           Control.Lens                     hiding (from, snoc, ( # ))
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Semigroup
import qualified Data.Text.Lazy                   as T

import           Control.Applicative
import           Data.Char
import           Data.Foldable                    (Foldable (..))
import qualified Data.List                        as List
import           Data.Typeable                    (Typeable)
import           GHC.Exts                         (IsString (..),IsList (..))
import           Prelude                          hiding (foldr1, mapM)


-- ================================================================ --
-- Phrase_ types

type Phrase' = [Phrase_]

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
 deriving (Show,Eq,Ord,Data)

data Casing = Upper | Lower | Capper deriving (Show,Eq,Ord,Enum,Data)
data Joiner = Joiner String | CamelJoiner | ClassJoiner deriving (Show,Eq,Ord,Data)
data Brackets = Brackets String String deriving (Show,Eq,Ord,Data)
newtype Separator = Separator String  deriving (Show,Eq,Ord,Data)
type Keyword = String -- TODO
newtype Dictation = Dictation [String] deriving (Show,Eq,Ord,Data)

instance IsString Dictation where
 fromString = Dictation . words               -- safe: words "" == []

instance IsList Dictation where
 type Item Dictation = String
 fromList = Dictation
 toList (Dictation ws) = ws

word2phrase_ = Dictated_ . Dictation . (:[])


-- not {JoinerFunction ([String] -> String)} to keep the {Eq} instance for caching
-- fake equality? {JoinerFunction Name ([String] -> String)} so {JoinerFunction 'camelCase camelCase}
-- maybe some type from data.split package, that both supports decidable equality and that can build functions


-- ================================================================ --
-- Phrase types

-- | user-facing Phrase. exists at (DSL) parse-time.
type Phrase  = PhraseF (Either Pasted PAtom)
-- | "Mungeable Phrase". exists only at (DSL) run-time.
-- the atom is really a list of atoms, but not a full Sexp. this supports "splatting". We interpret a list of atoms as string of words with white space between, more or less.
type MPhrase = PhraseF [PAtom]
type PhraseF = Sexp PFunc

-- | a "Phrase Function".
data PFunc
 = Cased      Casing
 | Joined     Joiner
 | Surrounded Brackets
 deriving (Show,Eq,Ord,Data)

-- | "Phrase Atom".
--
-- 'PAcronym's behave differently from 'PWord's under some 'Joiner's (e.g. class case).
-- a 'PAcronym' should hold only uppercase letters.
data PAtom
 = PWord String
 | PAcronym [Char]
 deriving (Show,Eq,Ord,Data)

-- | for doctest
instance IsString PAtom where fromString = PWord

{- | a Pasted is like a 'Dictation'/'Quoted_' i.e. a list of words, not a single word.

we know (what words are in) the Dictation at (DSL-)"parse-time" i.e. 'phrase',
but we only know (what words are in) the Pasted at (DSL-)"runtime"
(i.e. wrt the DSL, not Haskell). Thus, it's a placeholder.

-}
data Pasted = Pasted  deriving (Show,Eq,Ord,Data)

-- | used by 'pPhrase'.
type PStack = NonEmpty PItem
-- -- the Left represents 'List', the Right represents 'Sexp', 'Atom' is not represented.
-- type PStack = NonEmpty (Either [Phrase] (PFunc, [Phrase]))

-- | an inlined subset of 'Sexp'.
--
-- Nothing represents 'List', Just represents 'Sexp', 'Atom' is not represented.
type PItem = (Maybe PFunc, [Phrase])




-- ================================================================ --

-- -- | transforms "token"s from 'phrase_' into an "s-expression" with 'pPhrase'.
-- phrase = pPhrase <$> phrase_

phrase_ :: DNSEarleyRHS z Phrase'
phrase_ = complexGrammar 'phrase_
-- (snoc     <$>             ((phraseA <|> phraseB <|> phraseW)-*) <*> (phraseB <|> phraseC <|> phraseD))
 (conssnoc <$> (phraseA) <*> ((phraseA <|> phraseB <|> phraseW)-*) <*> (phraseB <|> phraseC <|> phraseD))
 (conssnoc <$> (phraseA) <*> ((phraseA <|> phraseB <|> phraseD)-*) <*> (phraseB <|> phraseC <|> phraseD))
 where
 conssnoc x ys z = [x] <> ys <> [z]

-- | a sub-phrase where a phrase to the right is certain.
--
-- this ordering prioritizes the escaping Escaped_/Quoted_ over the
-- escaped, e.g. "quote greater equal unquote".
phraseA :: DNSEarleyRHS z Phrase_
phraseA = 'phraseA <=> empty
 <|> Escaped_    <#> "lit" # keyword
 <|> Quoted_     <#> "quote" # dictation # "unquote"
 <|> Pasted_     <#> "paste"
 <|> Blank_      <#> "blank"
 -- <|> (Spelled_ . (:[])) <$> char --TODO
 -- <|> (Spelled_) <#> letter_
 -- <|> (Spelled_ . (:[])) <#> character
 <|> Spelled_    <#> "spell" # (character-++)
 <|> Spelled_    <#> "lets" # letters -- (letter-++)
 <|> Separated_  <#> separator
 <|> Cased_      <#> casing
 <|> Joined_     <#> joiner
 <|> Surrounded_ <#> brackets

-- | a sub-phrase where a phrase to the right is possible.
phraseB :: DNSEarleyRHS z Phrase_
phraseB = 'phraseB <=> empty
 <|> Spelled_  <#> "spell" # (character-++)
 <|> Capped_   <#> "caps" # (character-++)
 -- <$> alphabetRHS
 -- TODO letters grammar that consumes tokens with multiple capital letters, as well as tokens with single aliases
 -- <|> Spelled_  <#> "spell" # letters -- only, not characters

-- | a sub-phrase where a phrase to the right is impossible.
phraseC :: DNSEarleyRHS z Phrase_
phraseC = 'phraseC <=> Dictated_ <#> "say" # dictation

-- | injects word_ into phrase_
phraseW :: DNSEarleyRHS z Phrase_
phraseW = 'phraseW <=> word2phrase_ <#> word_

-- | injects dictation into phrase_
phraseD :: DNSEarleyRHS z Phrase_
phraseD = 'phraseD <=> Dictated_ <#> dictation

separator = 'separator <=> empty
 <|> Separator ""  <#> "break" --TODO separation should depend on context i.e. blank between symbols, a space between words, space after a comma but not before it. i.e. the choice is delayed until munging.
 <|> Separator " " <#> "space"
 <|> Separator "," <#> "comma"

casing = enumGrammar

joiner = 'joiner
 <=> (\c -> Joiner [c]) <#> "join" # character
 <|> Joiner "_" <#> "snake"
 <|> Joiner "-" <#> "dash"
 <|> Joiner "/" <#> "file"
 <|> Joiner ""  <#> "squeeze"
 <|> CamelJoiner <#> "cam"
 <|> ClassJoiner <#> "class"

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

-- disjoint vocabulary ("effects"), possibly overlapping parses ("results")
character :: DNSEarleyRHS z Char
character = 'character <=> empty
 <|> punctuationRHS
 <|> englishNumericRHS
 <|> literalNumericRHS
 <|> phoneticAlphabetRHS
-- <|> shortAlphabetRHS
-- <|> literalAlphabetRHS

punctuationRHS :: DNSEarleyRHS z Char
punctuationRHS = vocab
 [ "grave"-: '`'
 , "till"-: '~'
 , "bang"-: '!'
 , "axe"-: '@'
 , "pound"-: '#'
 , "doll"-: '$'
 , "purse"-: '%'
 , "care"-: '^'
 , "amp"-: '&'
 , "star"-: '*'
 , "lore"-: '('
 , "roar"-: ')'
 , "score"-: '_'
 , "eek"-: '='
 , "plus"-: '+'
 , "lack"-: '['
 , "lace"-: '{'
 , "rack"-: ']'
 , "race"-: '}'
 , "stroke"-: '\\'
 , "pipe"-: '|'
 , "sem"-: ';'
 , "coal"-: ':'
 , "tick"-: '\''
 , "quote"-: '"'
 , "com"-: ','
 , "less"-: '<'
 , "dot"-: '.'
 , "great"-: '>'
 , "slash"-: '/'
 , "quest"-: '?'
 , "tab"-: '\t'
 , "ace"-: ' '
 , "line"-: '\n'
 ]

englishNumericRHS :: DNSEarleyRHS z Char
englishNumericRHS = vocab
 [ "zero"-: '0'
 , "one"-: '1'
 , "two"-: '2'
 , "three"-: '3'
 , "four"-: '4'
 , "five"-: '5'
 , "six"-: '6'
 , "seven"-: '7'
 , "eight"-: '8'
 , "nine"-: '9'
 ]

-- | @('read' <$> digits :: R_ 'Int')@ is total.
digits = 'digits <=> (digit-++)

digit :: R z Char
digit = 'digit <=> (head . show) <$> digitRHS

digitRHS :: (Num a) => R z a
digitRHS = vocab
 [ "nil"   -: 0                  -- monosyllabic
 , "zero"  -: 0                 -- disyllabic
 , "one"   -: 1
 , "two"   -: 2
 , "three" -: 3
 , "four"  -: 4
 , "five"  -: 5
 , "six"   -: 6
 , "sev"   -: 7                  -- monosyllabic
 , "seven" -: 7                -- disyllabic
 , "eight" -: 8
 , "nine"  -: 9
 ]

type Number = Int

number :: R z Number
number = 'number <=> numberRHS

numberRHS :: (Num a) => R z a
numberRHS = digitRHS <|> vocab
 [ "ten"-: 10
 , "eleven"-: 11
 , "twelve"-: 12
 , "thirteen"-: 13
 , "fourteen"-: 14
 , "fifteen"-: 15
 , "sixteen"-: 16
 , "seventeen"-: 17
 , "eighteen"-: 18
 , "nineteen"-: 19
 , "twenty"-: 20
 , "twenty-one"-: 21
 , "twenty-two"-: 22
 , "twenty-three"-: 23
 , "twenty-four"-: 24
 , "twenty-five"-: 25
 , "twenty-six"-: 26
 , "twenty-seven"-: 27
 , "twenty-eight"-: 28
 , "twenty-nine"-: 29
 , "thirty"-: 30
 , "thirty-one"-: 31
 , "thirty-two"-: 32
 , "thirty-three"-: 33
 , "thirty-four"-: 34
 , "thirty-five"-: 35
 , "thirty-six"-: 36
 , "thirty-seven"-: 37
 , "thirty-eight"-: 38
 , "thirty-nine"-: 39
 , "forty"-: 40
 , "forty-one"-: 41
 , "forty-two"-: 42
 , "forty-three"-: 43
 , "forty-four"-: 44
 , "forty-five"-: 45
 , "forty-six"-: 46
 , "forty-seven"-: 47
 , "forty-eight"-: 48
 , "forty-nine"-: 49
 , "fifty"-: 50
 , "fifty-one"-: 51
 , "fifty-two"-: 52
 , "fifty-three"-: 53
 , "fifty-four"-: 54
 , "fifty-five"-: 55
 , "fifty-six"-: 56
 , "fifty-seven"-: 57
 , "fifty-eight"-: 58
 , "fifty-nine"-: 59
 , "sixty"-: 60
 , "sixty-one"-: 61
 , "sixty-two"-: 62
 , "sixty-three"-: 63
 , "sixty-four"-: 64
 , "sixty-five"-: 65
 , "sixty-six"-: 66
 , "sixty-seven"-: 67
 , "sixty-eight"-: 68
 , "sixty-nine"-: 69
 , "seventy"-: 70
 , "seventy-one"-: 71
 , "seventy-two"-: 72
 , "seventy-three"-: 73
 , "seventy-four"-: 74
 , "seventy-five"-: 75
 , "seventy-six"-: 76
 , "seventy-seven"-: 77
 , "seventy-eight"-: 78
 , "seventy-nine"-: 79
 , "eighty"-: 80
 , "eighty-one"-: 81
 , "eighty-two"-: 82
 , "eighty-three"-: 83
 , "eighty-four"-: 84
 , "eighty-five"-: 85
 , "eighty-six"-: 86
 , "eighty-seven"-: 87
 , "eighty-eight"-: 88
 , "eighty-nine"-: 89
 , "ninety"-: 90
 , "ninety-one"-: 91
 , "ninety-two"-: 92
 , "ninety-three"-: 93
 , "ninety-four"-: 94
 , "ninety-five"-: 95
 , "ninety-six"-: 96
 , "ninety-seven"-: 97
 , "ninety-eight"-: 98
 , "ninety-nine"-: 99
 , "one-hundred"-: 100
 ]

{- | equivalent to:

@
 <|> '0' <$ "0"
 <|> '1' <$ "1"
 ...
 <|> '9' <$ "9"
@

-}
literalNumericRHS :: DNSEarleyRHS z Char
literalNumericRHS = foldMap (\c -> c <$ token [c]) ['0'..'9']

shortAlphabetRHS :: DNSEarleyRHS z Char
shortAlphabetRHS = vocab
 [ "ay"-: 'a'
 , "bee"-: 'b'
 , "sea"-: 'c'
 , "dee"-: 'd'
 , "eek"-: 'e'
 , "eff"-: 'f'
 , "gee"-: 'g'
 , "aych"-: 'h'
 , "eye"-: 'i'
 , "jay"-: 'j'
 , "kay"-: 'k'
 , "el"-: 'l'
 , "em"-: 'm'
 , "en"-: 'n'
 , "oh"-: 'o'
 , "pea"-: 'p'
 , "queue"-: 'q'
 , "are"-: 'r'
 , "ess"-: 's'
 , "tea"-: 't'
 , "you"-: 'u'
 , "vee"-: 'v'
 , "dub"-: 'w'
 , "ex"-: 'x'
 , "why"-: 'y'
 , "zee"-: 'z'
 ]

phoneticAlphabetRHS :: DNSEarleyRHS z Char
phoneticAlphabetRHS = vocab
 [ "alpha"-: 'a'
 , "bravo"-: 'b'
 , "charlie"-: 'c'
 , "delta"-: 'd'
 , "echo"-: 'e'
 , "foxtrot"-: 'f'
 , "golf"-: 'g'
 , "hotel"-: 'h'
 , "india"-: 'i'
 , "juliet"-: 'j'
 , "kilo"-: 'k'
 , "lima"-: 'l'
 , "mike"-: 'm'
 , "november"-: 'n'
 , "oscar"-: 'o'
 , "poppa"-: 'p'
 , "quebec"-: 'q'
 , "romeo"-: 'r'
 , "sierra"-: 's'
 , "tango"-: 't'
 , "uniform"-: 'u'
 , "victor"-: 'v'
 , "whiskey"-: 'w'
 , "x-ray"-: 'x'
 , "yankee"-: 'y'
 , "zulu"-: 'z'
 ]

{- | equivalent to:

@
 <|> 'a' <#> "a"
 <|> 'b' <#> "b"
 <|> 'c' <#> "c"
 <|> ...
 <|> 'z' <#> "z"

 <|> 'a' <#> "A"
 <|> 'b' <#> "B"
 <|> 'c' <#> "C"
 <|> ...
 <|> 'z' <#> "Z"
@

-}
literalAlphabetRHS :: DNSEarleyRHS z Char
literalAlphabetRHS = foldMap (\c -> (c <$ token [c]) <|> (c <$ token [toUpper c])) ['a'..'z'] -- TODO What will we get back from Dragon anyway?

dictation = dragonGrammar 'dictation
 ((Dictation . fmap T.unpack) <$> some anyWord)
 (DGNDictation)
{-# NOINLINE dictation #-} --TODO doesn't help with the unshared <dictation__4>/<dictation__14>/<dictation__16>

word_ = dragonGrammar 'word_
 (T.unpack <$> anyWord)
 (DGNWords)

keyword = dragonGrammar 'keyword
 (T.unpack <$> anyWord)
 (DGNWords)
 -- <=> Keyword <$> word_

letters = simpleGrammar 'letters
 ((T.unpack . fold) <$> some anyWord)
 (DNSMultiple $ SomeDNSNonTerminal $ DNSBuiltinRule $ DGNLetters)

-- newtype Letters = Letters [Char] deriving (Show,Eq,Ord)
-- letters = (set dnsInline True defaultDNSInfo) $ 'letters <=>
--  Letters <#> (letter-+)




-- ================================================================ --

asPhrase :: String -> Phrase
asPhrase = Atom . Right . PWord

bracket :: Char -> Brackets
bracket c = Brackets [c] [c]

-- | splats the Pasted into PAtom's, after splitting the clipboard into words
splatPasted :: Phrase -> OSX.ClipboardText -> MPhrase
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
caseWith c = mapPAtom (fromCasing c)

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

phraseCommand :: DNSEarleyCommand z [Phrase_]
phraseCommand = Command phrase_ (argmax rankPhrase) $ \_ p -> do
 s <- OSX.getClipboard
 OSX.sendText (runPhrase_ defSpacing s p)

runPhrase_ :: Spacing -> OSX.ClipboardText -> [Phrase_] -> String
runPhrase_ spacing clipboard
 = flip(mungePhrase) spacing
 . flip(splatPasted) clipboard
 . pPhrase

bestPhrase :: NonEmpty [Phrase_] -> [Phrase_]
bestPhrase = argmax rankPhrase

-- the specificity ("probability") of the phrase parts. bigger is better.
rankPhrase :: [Phrase_] -> Int
rankPhrase = sum . fmap (\case
 Escaped_ _ -> 2000
 Quoted_ _ -> 100
 Pasted_ -> 100
 Blank_ -> 100
 Spelled_ _ -> 100
 Capped_ _ -> 100
 Separated_ _ -> 100
 Cased_ _ -> 100
 Joined_ _ -> 100
 Surrounded_ _ -> 100
 Dictated_ (Dictation ws) -> length ws - 1  -- [Dictated_ ["some","words"]] =1 is better than [Dictated_ ["some"], Dictated_ ["words"]] =0
 )

-- -- | convenience function for testing how phrase_ parses
-- parsePhrase_ :: [Text] -> String
-- parsePhrase_
--  = runPhrase_ defSpacing "clipboard contents"
--  . argmax rankPhrase
--  . NonEmpty.fromList --  TODO
--  . parseList phrase_

