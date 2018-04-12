{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
-- | (Utility functions)
module Commands.Frontends.Dragon13.Extra
 ( module Commands.Frontends.Dragon13.Extra
 , module Data.Possibly
 , module Prelude.Spiros
 ) where

import Data.Possibly

import           Data.Bifoldable              (Bifoldable, bifoldMap)
import           Data.Bifunctor               (first)
import           Data.Either.Validation       (Validation, eitherToValidation)
import           Text.PrettyPrint.Leijen.Text (Doc, displayT, renderPretty)

import qualified Data.Set as Set
import           GHC.Exts                          (IsString (..))

import Prelude.Spiros hiding
 (constructors
 ,Possibly,throwM
 )
import Prelude(toEnum)

-- | The constructors of a (zero-based) Enum.
--
-- >>> constructors :: [Bool]
-- [False,True]
--
-- (Bounded Constraint elided for convenience; doesn't terminate on un@Bounded@ @Enum@erations)
--
constructors :: (Enum a) => [a]
constructors = enumFrom (toEnum 0)

-- | The first constructor of a (zero-based) Enum.
--
-- >>> enumDefault :: Bool
-- False
--
-- (Bounded Constraint elided for convenience; doesn't terminate on un@Bounded@ @Enum@erations)
--
enumDefault :: (Enum a) => a
enumDefault = toEnum 0

displayDoc :: Doc -> Text
displayDoc = displayT . renderPretty 1.0 80

-- | helper function to write manual Show instances.
 -- e.g. for existentially quantified types.
showsPrecNewtype :: (Show a) => Int -> String -> a -> ShowS
showsPrecNewtype depth name value = showParen
 (depth >= 11)
 (showString (name <> " ") . showsPrec (10+1) value)

-- | remove duplicates in @O(n log n)@ time.
-- <https://github.com/nh2/haskell-ordnub>
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

-- | @Either@ is a @Monad@: it short-circuits. 'Validation' is an @Applicative@, but not a @Monad@: under @traverse@ (or @bitraverse@), it runs the validation (@:: a -> f b@) on every field (@:: a@) in the traversable (@:: t a@), monoidally appending together all errors, not just the first.
eitherToValidations :: Either e a -> Validation [e] a
eitherToValidations = eitherToValidation . first (:[])

-- | a 'bifoldMap' on the left, removing duplicates.
--
--
--
--
getLefts :: (Ord n, Bifoldable p) => p n t -> [n]
getLefts = ordNub . bifoldMap (:[]) (const []) --TODO ordNub

-- | a 'bifoldMap' on the right, removing duplicates.
--
--
getRights :: (Ord t, Bifoldable p) => p n t -> [t]
getRights = ordNub . bifoldMap (const []) (:[]) --TODO ordNub

{- | for convenience when writing string dicts, lets you leave keys/values blank

@
filterBlanks
 [ "a"-: "b"
 , "b"-: "a"
 , ""-: "..."
 ]
@

evaluates to:

@
 [ "a"-: "b"
 , "b"-: "a"
 ]
@

-}
filterBlanks :: (IsString k, Eq k) => [(k, v)] -> [(k, v)]
filterBlanks = filter $ \case
 ("",_) -> False
 (_, _) -> True
