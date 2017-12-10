{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

{-|
-}
module Commands.Frontends.Dictation where

import           GHC.Exts                         (IsString (..),IsList (..))

import Prelude.Spiros
import Prelude()

{-|
-}
newtype Dictation = Dictation [String]
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable,Semigroup,Monoid)

instance IsString Dictation where
 fromString = words2dictation
 -- safe: words "" == []

instance IsList Dictation where
 type Item Dictation = String
 fromList = Dictation
 toList (Dictation ws) = ws

words2dictation :: String -> Dictation
words2dictation = Dictation . words

displayDictation :: Dictation -> String
displayDictation (Dictation ws) = unwords ws
