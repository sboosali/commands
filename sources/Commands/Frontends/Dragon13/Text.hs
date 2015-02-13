{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, RankNTypes                                  #-}
module Commands.Frontends.Dragon13.Text
 ( DNSText (..)
 , DNSName (..)
 -- * smart constructors
 , escapeDNSText
 , escapeDNSName
 -- * only destructors
 -- , pattern DNSText
 -- , pattern DNSName
 -- * re-export
 , Text
 ) where
import           Commands.Etc
import           Control.Applicative
import           Data.Char
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as T


newtype DNSName = DNSName Text deriving (Show, Eq, Ord)
-- pattern DNSName s <- DNSName_ s

newtype DNSText = DNSText Text deriving (Show, Eq, Ord)
-- pattern DNSText s <- DNSText_ s


-- | Its output should be:
--
-- * a valid Python identifier, to support its callback method.
-- * (in particular) safe to interpolate into a Python docstring
-- * (in particular) nonempty
--
-- >>> :set -XOverloadedStrings
-- >>> escapeDNSName "a_1"
-- DNSName "a_1"
-- >>> escapeDNSName "1_a"
-- *** Exception: user error (escapeDNSName)
-- >>> escapeDNSName "α"
-- *** Exception: user error (escapeDNSName)
-- >>> escapeDNSName "'''"
-- *** Exception: user error (escapeDNSName)
-- >>> escapeDNSName ""
-- *** Exception: user error (escapeDNSName)
--
-- TODO fails on DNSPronounced "-f" "force"
--
-- there are two safe text types:
-- the left-hand sides and the terminals...
-- maybe I should just validate when constructing the DNSGrammar,
-- with smart constructors, not after construction. but then
-- construction is a partial function
escapeDNSName :: Text -> Possibly DNSName
escapeDNSName s
 | isPythonIdentifier s = return . DNSName $ s
 | otherwise = failed "escapeDNSName"
 where
 isPythonIdentifier s = T.all isAscii s && case T.uncons s of
    Nothing    -> False
    Just (c,s) -> isAlpha c && T.all ((||) <$> isAlphaNum <*> (=='_')) s

-- | Its output should be:
--
-- * safe to interpolate into any Python docstring (double-quoted or
-- single-quoted)
--
-- * nonempty
--
--
escapeDNSText :: Text -> Possibly DNSText
escapeDNSText s
 | isValid s = return . DNSText $ s
 | otherwise = failed "escapeDNSText"
 where
 isValid s
  =  not (T.null s)
  && T.all isAscii s
  && T.all (not . (=='\n')) s
  && not ("'''" `T.isInfixOf` s)
  && not ("\"\"\"" `T.isInfixOf` s)
