{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, PatternSynonyms #-}
{-# LANGUAGE RankNTypes                                                 #-}
module Commands.Frontends.Dragon13.Text
 ( DragonText (..)
 -- * smart constructor
 , escapeDragonText
 -- * only destructor
 -- , pattern DragonText
 -- * re-export
 , Text
 ) where
import           Commands.Etc
import           Control.Applicative
import           Data.Char
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as T


newtype DragonText = DragonText Text
 deriving (Show, Eq, Ord)

-- | Its output should be:
--
-- * a valid Python identifier
-- * (in particular) safe to interpolate into a Python docstring
-- * (in particular) nonempty
--
-- >>> :set -XOverloadedStrings
-- >>> escapeDragonText "a_1"
-- DragonText "a_1"
-- >>> escapeDragonText "1_a"
-- *** Exception: user error (escapeDragonText)
-- >>> escapeDragonText "α"
-- *** Exception: user error (escapeDragonText)
-- >>> escapeDragonText "'''"
-- *** Exception: user error (escapeDragonText)
-- >>> escapeDragonText ""
-- *** Exception: user error (escapeDragonText)
--
-- TODO fails on DragonPronounced "-f" "force"
--
-- there are two safe text types:
-- the left-hand sides and the terminals...
-- maybe I should just validate when constructing the DragonGrammar,
-- with smart constructors, not after construction. but then
-- construction is a partial function
escapeDragonText :: Text -> Possibly DragonText
escapeDragonText s
 | isPythonIdentifier s = return $ DragonText s
 | otherwise = failed "escapeDragonText"
 where
 isPythonIdentifier s = T.all isAscii s && case T.uncons s of
    Nothing    -> False
    Just (c,s) -> isAlpha c && T.all ((||) <$> isAlphaNum <*> (=='_')) s

-- pattern DragonText s <- DragonText_ s

