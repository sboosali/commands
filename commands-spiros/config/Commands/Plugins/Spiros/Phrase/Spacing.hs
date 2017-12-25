{-# LANGUAGE LambdaCase, MultiWayIf, TupleSections  #-}
module Commands.Plugins.Spiros.Phrase.Spacing where

import Data.Char
import qualified Data.Map as Map

import Prelude.Spiros
import Prelude ()

{-| some text that's being spaced out.

(specialized @Reader@; simplifies refactoring.)

-}
type Spaced a = Spacing -> a

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





-- ================================================================ --

-- |
--
-- TODO prop> length xs < 2 ==> length (stagger xs) == 0
-- TODO prop> 2 <= length xs ==> length (stagger xs) + 1 == length xs
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
 go zs = concat . zipWith (\a b -> [a,b]) zs

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

data SpacingCategory = NoSpacing | LeftSpacing | RightSpacing | BothSpacing
 deriving (Show,Read,Eq,Ord,Enum,Bounded, Generic )

{-|

distinguish "spacing punctuation" from "raw punctuation"

e.g. in English, the colon is followed on the right by a space before the next word (it's a 'RightSpacing').
but in Haskell, the double-colon is an operator with no spaces in between.
and for magiccards.info, there's no spacing between the selector and the rest of the query.

-}
getSpacingCategory :: Char -> SpacingCategory
getSpacingCategory = ((Map.findWithDefault NoSpacing)&flip) _mapping
 where
 _mapping = _splatMapping _categories
 _splatMapping = concatMap (\(ks,v) -> fmap (,v) ks ) > Map.fromList
 _categories =
  [ ['/'] -: NoSpacing
  , [',', '.', '?', '!', ':', ';'] -: RightSpacing
  , [')', ']', '}' ] -: RightSpacing
  , ['(', '[', '{'] -: LeftSpacing
  , ['&'] -: BothSpacing
  ]

spaceOut :: [String] -> Spaced String -- TODO Either String
-- spaceOut xs _spacing = List.intercalated " " xs
spaceOut xs spacing = concat (fmap (either id (getSpacing spacing)) (interstagger xs))
