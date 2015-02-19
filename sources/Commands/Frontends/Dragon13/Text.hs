{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, RankNTypes                                  #-}
module Commands.Frontends.Dragon13.Text
 ( DNSName (..)
 , DNSText (..)
 -- * smart constructors
 , escapeDNSName
 , escapeDNSText
 -- * dumb constructors
 , unsafeDNSName
 , unsafeDNSText
 -- * predicates
 , isDNSName
 , isDNSText
 -- * only destructors
 -- , pattern DNSName
 -- , pattern DNSText
 -- * re-export
 , Text
 ) where
import           Commands.Etc
import           Control.Applicative
import           Data.Char
import           Data.Monoid         ((<>))
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as T


newtype DNSName = DNSName { unDNSName :: Text } deriving (Show, Eq, Ord)
-- pattern DNSName s <- DNSName_ s

newtype DNSText = DNSText { unDNSText :: Text } deriving (Show, Eq, Ord)
-- pattern DNSText s <- DNSText_ s

unsafeDNSName :: Text -> DNSName
unsafeDNSName = DNSName

unsafeDNSText :: Text -> DNSText
unsafeDNSText = DNSText


-- | Its output should be:
--
-- * a valid Python identifier, to support its callback method.
-- * (in particular) safe to interpolate into a Python docstring
-- * (in particular) nonempty
--
-- >>> :set -XOverloadedStrings
-- >>> escapeDNSName "a_1"
-- DNSName {unDNSName = "a_1"}
-- >>> escapeDNSName "1_a"
-- *** Exception: user error (escapeDNSName "1_a")
-- >>> escapeDNSName "α"
-- *** Exception: user error (escapeDNSName "\945")
-- >>> escapeDNSName "'''"
-- *** Exception: user error (escapeDNSName "'''")
-- >>> escapeDNSName ""
-- *** Exception: user error (escapeDNSName "")
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
 | isDNSName s = return . DNSName $ s
 | otherwise = failed $ "escapeDNSName " <> show s

isDNSName :: Text -> Bool
isDNSName s = T.all isAscii s && case T.uncons s of
 Nothing    -> False
 Just (c,s) -> isAlpha c && T.all ((||) <$> isAlphaNum <*> (=='_')) s

-- | Its output should be:
--
-- * safe to interpolate into any Python string (double-quoted or
-- single-quoted, docstring or not)
-- * the only whitespace are spaces
-- * nonempty
--
-- >>> :set -XOverloadedStrings
-- >>> escapeDNSText "some words"
-- DNSText {unDNSText = "some words"}
-- >>> escapeDNSText "1_a"
-- DNSText {unDNSText = "1_a"}
-- >>> escapeDNSText "some\nwords"
-- *** Exception: user error (escapeDNSText "some\nwords")
-- >>> escapeDNSText "α"
-- *** Exception: user error (escapeDNSText "\945")
-- >>> escapeDNSText "'"
-- *** Exception: user error (escapeDNSText "'")
-- >>> escapeDNSText "\""
-- *** Exception: user error (escapeDNSText "\"")
-- >>> escapeDNSText ""
-- *** Exception: user error (escapeDNSText "")
--
--
escapeDNSText :: Text -> Possibly DNSText
escapeDNSText s
 | isDNSText s = return . DNSText $ s
 | otherwise = failed $ "escapeDNSText " <> show s

isDNSText :: Text -> Bool
isDNSText s
  =  not (T.null s)
  && T.all isAscii s
  && T.all isPrint s
  && T.all (isSpace --> (==' ')) s
  && not ("'" `T.isInfixOf` s)
  && not ("\"" `T.isInfixOf` s)
  && not ("\\" `T.isInfixOf` s)
