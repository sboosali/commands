{-# LANGUAGE ViewPatterns #-}
module Commands.Test.Properties where
import           Commands.Etc
import           Commands.Frontends.Dragon13
import           Commands.Frontends.Dragon13.Text
import           Commands.Frontends.Dragon13.Types
import           Commands.Test.Arbitrary
import           Commands.Test.Types

import           Data.Bifunctor                    (bimap)
import qualified Data.Text.Lazy                    as T
import qualified Test.QuickCheck.Property          as Q


-- | if the grammar has already been escaped, its serialization into the shim must be valid Python.
prop_DNSGrammar :: DNSGrammar () DNSText DNSName -> Q.Result
prop_DNSGrammar grammar = case shimmySerialization address (serializeGrammar grammar) of
 Left  {} -> Q.failed
 Right {} -> Q.succeeded
-- rejected = result { ok = Nothing }  i.e.  Discard  i.e.  as if precondition were False
 where
 -- diff = (Address (T.pack "'localhost'") (T.pack "'8000'"), Address (T.pack "'localhost'") (T.pack "'8000'"))
 address = Address (Host "localhost") (Port 8000)
