{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, LambdaCase, MultiWayIf #-}
module Commands.Plugins.Example.Phrase where
import           Commands.Munging

import           Control.Lens.Plated
import           Data.Semigroup

import           Control.Applicative
import           Data.Char
import           Data.Foldable       (fold)
import qualified Data.List           as List
import           Data.Traversable
import           Data.Typeable       (Typeable)
import           GHC.Exts            (IsString (..))
import           Prelude             hiding (mapM)


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

-- | refines any Sexp to the List variant.
--
-- you can safely pattern match on it: @let 'List' xs = toSexpList x@ succeeds forall @x@.
toSexpList :: Sexp f a -> Sexp f a
toSexpList = \case
 List ss -> List ss
 s       -> List [s]
{-# INLINE toSexpList #-}

-- |
appendSexp :: Sexp f a -> Sexp f a -> Sexp f a
appendSexp x y = List (xs <> ys)
 where
 List xs = toSexpList x
 List ys = toSexpList y
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
-- e.g.:
--
-- * we should put spaces between words, but not between
-- punctuation
-- * only after a comma, but both before/after an equals sign.
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

instance IsString PAtom where fromString = PWord -- ^ for doctest

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

