{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, LambdaCase, MultiWayIf, TemplateHaskell, PostfixOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Commands.Plugins.Example.Phrase where
import           Commands.Munging
import           Commands.Core
import           Commands.Frontends.Dragon13

import           Control.Lens.Plated
import           Data.Semigroup
import           Data.List.NonEmpty              (NonEmpty (..), fromList)
import qualified Data.List.NonEmpty              as NonEmpty
import           Control.Lens                    hiding (from, ( # ), (&))

import           Data.Foldable                   (Foldable (..), asum)
import           Control.Applicative
import           Data.Char
import qualified Data.List           as List
import           Data.Traversable
import           Data.Typeable       (Typeable)
import           GHC.Exts            (IsString (..))
import           Prelude             hiding (mapM, foldr1)


-- |
--
-- prop> length xs < 2 ==> length (stagger xs) == 0
-- prop> 2 <= length xs ==> length (stagger xs) + 1 == length xs
stagger :: [a] -> [(a,a)]
stagger []  = []
stagger [_] = []
stagger xs@(_:_) = zip (init xs) (tail xs)

-- |
--
interstagger :: [a] -> [Either a (a,a)]
interstagger xs = interweave (Left <$> xs) (Right <$> stagger xs)

-- | >>> interweave (Left <$> [1,2,3]) (Right <$> "ab")
-- [Left 1,Right 'a',Left 2,Right 'b',Left 3]
--
-- the input @interweave xs ys@ should satisfy @length xs == 1 + length ys@.
-- may truncate @ys@.
-- guarantees that the whole input @xs@ exists as a
-- <http://en.wikipedia.org/wiki/Subsequence subsequence> in the output.
--
-- TODO prop> subsequenceOf xs (interweave xs ys)
-- TODO prop> forall nonempty xs ==> head xs == head (interweave xs ys)
-- TODO prop> forall nonempty xs ==> last xs == last (interweave xs ys)
--
--
--
interweave :: [a] -> [a] -> [a]
interweave [] _ = []
interweave (x:xs) ys = x : go (take (length xs) ys) xs -- forces both lists to share same length
 where
 go ys = concat . zipWith (\a b -> [a,b]) ys


{- | a heterogenous list.
a <http://en.wikipedia.org/wiki/Common_Lisp#The_function_namespace Lisp-2> S-expression, where:

* @f@ is the function namespace
* @a@ is the atom namespace

you could @type Lisp1 a = Sexp a a@, but:

* @f@ is ignored by 'Monad'ic methods like 'joinSexp'
* you might want to make sure to eval the @f@ to some normal form,
like when matching against it in 'evalSexp'
* @plate@ doesn't reach the @f@, even when @f ~ a@,
as the 'Plated' instance is manual, not automatic via @Data@.

'List' is just a specialized @'Sexp' ()@, but easier to work with than:

* @Sexp (Maybe f) [Sexp f a]@ (where Nothing would represent 'List')
* forcing each concrete @f@ to hold a unit (which would represent 'List')


-}
data Sexp f a
 = Atom a
 | List   [Sexp f a]
 | Sexp f [Sexp f a]
 deriving (Show,Eq,Ord, Functor)

-- | default from the 'Monad' subclass.
instance Applicative (Sexp f) where
 pure = return
 (<*>) f x = f >>= (\g -> x >>= (return.g))

{- |

@
 'return' = 'returnSexp'
 '(>>=)' = 'bindSexp'
@

proofs of laws:


* TODO : @join . return = id@

@
joinSexp (returnSexp m)
joinSexp (Atom m)
m
@


* TODO : @join . fmap return = id@

@
joinSexp (fmap returnSexp m)
TODO

m
@


* associativity: @join . join = join . fmap join@

@
TODO
@



laws, verified as @QuickCheck@ properties:

TODO


-}
instance Monad (Sexp f) where
 return = returnSexp
 (>>=) = bindSexp

instance Semigroup (Sexp f a) where (<>)   = appendSexp
instance Monoid    (Sexp f a) where mempty = nilSexp; mappend = (<>)

instance Plated (Sexp f a) where
 plate f = \case
  Atom a    -> Atom   <$> pure a
  List   ps -> List   <$> traverse f ps
  Sexp g ps -> Sexp g <$> traverse f ps



returnSexp :: a -> Sexp f a
returnSexp = Atom
{-# INLINE returnSexp #-}

bindSexp :: Sexp f a -> (a -> Sexp f b) -> Sexp f b
bindSexp s f = (joinSexp . fmap f) s
{-# INLINE bindSexp #-}

joinSexp :: Sexp f (Sexp f a) -> Sexp f a
joinSexp = \case
 Atom     s -> s
 List   sss -> List   (joinSexp <$> sss)
 Sexp f sss -> Sexp f (joinSexp <$> sss)
{-# INLINEABLE joinSexp #-} -- to hit the Atom I hope.

-- | refines any Sexp to a list, which can be given to the List variant.
--
toSexpList :: Sexp f a -> [Sexp f a]
toSexpList = \case
 List ss -> ss
 s       -> [s]
{-# INLINE toSexpList #-}

-- |
appendSexp :: Sexp f a -> Sexp f a -> Sexp f a
appendSexp x y = List (toSexpList x <> toSexpList y)
{-# INLINE appendSexp #-}

nilSexp :: Sexp f a
nilSexp = List []
{-# INLINE nilSexp #-}

-- | strictly evaluate a sexp ("all the way") to an atom.
--
-- e.g. when specializing @f ~ ((->) e)@, then
--
-- @evalSexp :: ([a] -> e -> a) -> ([a] -> g -> e -> a) -> Sexp g a -> (e -> a)@
--
-- and evaluation just takes an environment, passing it down to the two evaluators.
--
evalSexp :: (Monad m) => ([a] -> m a) -> ([a] -> g -> m a) -> Sexp g a -> m a
evalSexp list apply = \case
 Atom a    -> return a
 List   ss -> list           =<< mapM (evalSexp list apply) ss
 Sexp g ss -> (flip apply) g =<< mapM (evalSexp list apply) ss

-- |
--
-- when a Sexp\'s atoms are 'Monoid'al ("list-like"),
-- after evaluating some expressions into atoms,
-- we can "splat" them back together.
--
-- @splatList@ takes: a monadic evaluator @eval@
-- and a list of s-expressions @ss@ to evaluate in sequence.
splatList :: (Applicative m, Monoid xs) => (Sexp g xs -> m xs) -> [Sexp g xs] -> m xs
splatList eval ss = fold <$> traverse eval ss

-- |
--
-- e.g. @evalSplatSexp :: ([x] -> g -> Maybe [x]) -> (Sexp g [x] -> Maybe [x])@
--
evalSplatSexp :: (Applicative m, Monad m, Monoid xs) => (xs -> g -> m xs) -> (Sexp g xs -> m xs)
evalSplatSexp apply = evalSexp (return.fold) (apply.fold)



-- | "Phrase Spacing".
--
-- configuration for combining adjacent words/symbols.
--
-- see 'defSpacing' for an example.
newtype Spacing = Spacing { getSpacing :: SpacingX -> String }
-- type Spacing = (Map SpacingX String, String)
-- TODO replace the Map with generic "lookup" i.e. any function into
-- Maybe
-- Or a function from pairs of tokens to a separator

-- | "Spacing conteXt".
--
-- given a pair of words/symbols, how do we space them out when concatenating them?
--
type SpacingX = (String, String)

-- | (specialized @Reader@; simplifies refactoring.)
type Spaced a = Spacing -> a

-- |
--
-- we should put spaces:
--
-- * between words, but not between punctuation
-- * after a comma, but both before/after an equals sign.
--
defSpacing :: Spacing
defSpacing = Spacing $ \(l,r) -> if
 | null l || null r                     -> ""
 --- | all isAlphaNum l && all isAlphaNum r -> " "
 | isAlphaNum (last l) && isAlphaNum (head r) -> " "  -- earlier proven nonempty
 | l == ","                             -> " "
 | l == "=" || r == "="                 -> " "
 | otherwise                            -> ""
 -- cases should be disjoint (besides the last) for clarity

spaceOut :: [String] -> Spaced String
spaceOut xs spacing = concat $ fmap (either id (getSpacing spacing)) (interstagger xs)



-- | user-facing Phrase. exists at (DSL) parse-time.
type Phrase  = PhraseF (Either Pasted PAtom)
-- | "Mungeable Phrase". exists at (DSL) run-time.
-- the atom is really a list of atoms, but not a full Sexp. this supports "splatting". We interpret a list of atoms as string of words with white space between, more or less.
type MPhrase = PhraseF [PAtom]
type PhraseF = Sexp PFunc

asPhrase :: String -> Phrase
asPhrase = Atom . Right . PWord

-- | "Phrase Function".
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

we know the Dictation at "parse time" i.e. 'phrase', but we only know the Pasted at
"runtime" (wrt the DSL, not Haskell). Thus, it's a placeholder.


-}
data Pasted = Pasted  deriving (Show,Eq,Ord)

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






















newtype Dictation = Dictation [String] deriving (Show,Eq,Ord)
dictation = dragonGrammar 'dictation
 (DNSNonTerminal (SomeDNSLHS (DNSBuiltinRule DGNDictation)))
 (\context -> Dictation <$> anyBlack `manyUntil` context)

word_ = dragonGrammar 'word_
 (DNSNonTerminal (SomeDNSLHS (DNSBuiltinRule DGNWords)))
 (\_ -> anyWord)

letter_ :: Grammar Char
letter_ = dragonGrammar 'letter_
 (DNSNonTerminal (SomeDNSLHS (DNSBuiltinRule DGNLetters)))
 (\_ -> spaced anyLetter)

-- newtype Letters = Letters [Char] deriving (Show,Eq,Ord)
-- letters = (set dnsInline True defaultDNSInfo) $ 'letters <=>
--  Letters # (letter-+)
 -- TODO greedy (many) versus non-greedy (manyUntil)

-- |
-- TODO spacing, casing, punctuation; are all weird when letters are recognized by Dragon NaturallySpeaking.
anyLetter = oneOf ['A'..'Z']
-- anyLetter = anyChar
-- anyLetter = (\c -> c <$ [toUpper c]) <$> ['a'..'z'])

-- |
--
-- 'Phrase_' is the unassociated concrete syntax list
-- (e.g. tokens, parentheses),
-- while 'Phrase' is the associated abstract syntax tree (e.g. s-expressions).
data Phrase_
 = Escaped_  Keyword -- ^ atom-like.
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

joinSpelled :: [Phrase_] -> [Phrase_]
joinSpelled = foldr' go []
 where
 go :: Phrase_ -> [Phrase_] -> [Phrase_]
 go (Spelled_ xs) (Spelled_ ys : ps) = (Spelled_ $ xs <> ys) : ps
 go p ps = p:ps

-- | parses "tokens" into an "Sexp". a total function.
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

-- | its custom parser and grammar are implemented differently, but should behave consistently.
--
-- (its special parser threads the context differently than the generic parser would.)
--
-- TODO erase this horror from time
--
-- transforms "token"s from 'phrase_' into an "s-expression" with 'pPhrase'.
phrase = pPhrase <$> phrase_
phrase_ = Grammar
 (Rule l dependencies)
 (defaultDNSCommandProduction l gP)
 (\context -> try (pP context <?> showLHS l))

 where
 Just l = lhsFromName 'phrase
 dependencies = [] <$ liftGrammar (phraseA `eitherG` phraseB `eitherG` phraseC `eitherG` dictation)
 -- TODO the RHS of special grammars are ignored (so the eithers don't matter), except for extracting dependencies for serialization

 pP context                     -- merges the context-free Parsec.many the context-sensitive manyUntil
    = ([]    <$  (case context of Some q -> (try . lookAhead) q *> pure (error "pP"))) -- terminate.
  <|> ((:)   <$> pAB             <*> pP context)  -- continue. e.g. can escape "say" with "lit"
  <|> ((:[]) <$> pC context) -- terminate.
  <|> ((:)   <$> (pDxAB context) <*> pP context)  -- continue
 pAB = pA <|> pB
 pDxAB context = Dictated_ <$> pD (case context of Some q -> Some (pAB <|> (q *> pure (error "pDxAB"))))
 -- pAB context = (pA context <||> pB context)
 -- pD'AB context = ((Right . Dictated) <$> pD) `manyUntil` (pAB <|> context)
 pA         = try $ phraseA   ^. gramParser $ (error "pA") -- context free
 pB         = try $ phraseB   ^. gramParser $ (error "pB") -- context free
 pC context = try $ phraseC   ^. gramParser $ context             -- context-sensitive
 pD context = try $ dictation ^. gramParser $ context            -- context-sensitive
 -- pB' = (Dictated . Dictation) <$> anyWord `manyUntil` pB

 gP = (DNSSequence $ fromList
  [ (DNSOptional . DNSMultiple) (DNSAlternatives $ fromList [gA, gB, gD])
  ,                              DNSAlternatives $ fromList [gC, gB, gD]
  ])
 gA = (DNSNonTerminal . SomeDNSLHS) $ phraseA   ^. gramGrammar.dnsProductionLHS
 gB = (DNSNonTerminal . SomeDNSLHS) $ phraseB   ^. gramGrammar.dnsProductionLHS
 gC = (DNSNonTerminal . SomeDNSLHS) $ phraseC   ^. gramGrammar.dnsProductionLHS
 gD = (DNSNonTerminal . SomeDNSLHS) $ dictation ^. gramGrammar.dnsProductionLHS

-- | a sub-phrase where a phrase to the right is certain.
--
-- this ordering prioritizes the escaping Escaped_/Quoted_ over the
-- escaped, e.g. "quote greater equal unquote".
phraseA = 'phraseA <=> empty
 <|> Escaped_    # "lit" & keyword
 <|> Quoted_     # "quote" & dictation & "unquote"
 <|> Pasted_     # "paste"
 <|> Blank_      # "blank"
 <|> (Spelled_ . (:[])) # letter_
 <|> (Spelled_ . (:[])) # character
 <|> Separated_  # separator
 <|> Cased_      # casing
 <|> Joined_     # joiner
 <|> Surrounded_ # brackets
-- | a sub-phrase where a phrase to the right is possible.
phraseB = 'phraseB <=> empty
 -- TODO letters grammar that consumes tokens with multiple capital letters, as well as tokens with single aliases
 -- <|> Spelled_  # "spell" & letters -- only, not characters
 <|> Spelled_  # "spell" & (character-+)
 <|> Capped_   # "caps" & (character-+)
 -- <$> alphabetRHS
-- | a sub-phrase where a phrase to the right is impossible.
phraseC = 'phraseC <=> Dictated_ # "say" & dictation
-- TODO maybe consolidate phrases ABC into a phrase parser, with the same grammar, but which injects different constructors i.e. different views into the same type

type Keyword = String -- TODO
keyword :: Grammar Keyword
keyword = 'keyword <=>id#word_

newtype Separator = Separator String  deriving (Show,Eq,Ord)
separator = 'separator <=> empty
 <|> Separator ""  # "break" -- separation should depend on context i.e. blank between symbols, a space between words, space after a comma but not before it. i.e. the choice is delayed until munging.
 <|> Separator " " # "space"
 <|> Separator "," # "comma"

casing = enumGrammar
joiner = 'joiner
 <=> (\c -> Joiner [c]) # "join" & character
 <|> Joiner "_" # "snake"
 <|> Joiner "-" # "dash"
 <|> Joiner "/" # "file"
 <|> Joiner ""  # "squeeze"
 <|> CamelJoiner # "camel"
 <|> ClassJoiner # "class"
brackets = 'brackets
 <=> bracket          # "round" & character
 <|> Brackets "(" ")" # "par"
 <|> Brackets "[" "]" # "square"
 <|> Brackets "{" "}" # "curl"
 <|> Brackets "<" ">" # "angle"
 <|> bracket '"'      # "string"
 <|> bracket '\''     # "ticked"
 <|> bracket '|'      # "norm"
 -- <|> Brackets "**" "**" # "bold"

character :: Grammar Char
character = 'character <=> empty

 <|> '`' # "grave"
 <|> '~' # "till"
 <|> '!' # "bang"
 <|> '@' # "axe"
 <|> '#' # "pound"
 <|> '$' # "doll"
 <|> '%' # "purse"
 <|> '^' # "care"
 <|> '&' # "amp"
 <|> '*' # "star"
 <|> '(' # "lore"
 <|> ')' # "roar"
 <|> '-' # "dash"
 <|> '_' # "score"
 <|> '=' # "eek"
 <|> '+' # "plus"
 <|> '[' # "lack"
 <|> '{' # "lace"
 <|> ']' # "rack"
 <|> '}' # "race"
 <|> '\\' # "stroke"
 <|> '|' # "pipe"
 <|> ';' # "sem"
 <|> ':' # "coal"
 <|> '\'' # "tick"
 <|> '"' # "quote"
 <|> ',' # "com"
 <|> '<' # "less"
 <|> '.' # "dot"
 <|> '>' # "great"
 <|> '/' # "slash"
 <|> '?' # "quest"
 <|> ' ' # "ace"
 <|> '\t' # "tab"
 <|> '\n' # "line"

 <|> '0' # "zero"
 <|> '1' # "one"
 <|> '2' # "two"
 <|> '3' # "three"
 <|> '4' # "four"
 <|> '5' # "five"
 <|> '6' # "six"
 <|> '7' # "seven"
 <|> '8' # "eight"
 <|> '9' # "nine"

 <|> 'a' # "ay"
 <|> 'b' # "bee"
 <|> 'c' # "sea"
 <|> 'd' # "dee"
 <|> 'e' # "eek"
 <|> 'f' # "eff"
 <|> 'g' # "gee"
 <|> 'h' # "aych"
 <|> 'i' # "eye"
 <|> 'j' # "jay"
 <|> 'k' # "kay"
 <|> 'l' # "el"
 <|> 'm' # "em"
 <|> 'n' # "en"
 <|> 'o' # "oh"
 <|> 'p' # "pea"
 <|> 'q' # "queue"
 <|> 'r' # "are"
 <|> 's' # "ess"
 <|> 't' # "tea"
 <|> 'u' # "you"
 <|> 'v' # "vee"
 <|> 'w' # "dub"
 <|> 'x' # "ex"
 <|> 'y' # "why"
 <|> 'z' # "zee"
 <|> alphabetRHS


{- | equivalent to:

@
 <|> 'a' # "A"
 <|> 'b' # "B"
 <|> 'c' # "C"
 <|> ...
 <|> 'z' # "Z"
@

-}
alphabetRHS = (asum . List.map (\c -> c <$ liftString [toUpper c]) $ ['a'..'z'])
-- TODO What will we get back from Dragon anyway?
