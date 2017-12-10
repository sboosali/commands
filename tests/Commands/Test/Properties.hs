{-# LANGUAGE ViewPatterns #-}
module Commands.Test.Properties where
import           Commands.Extra
import           Commands.Frontends.Dragon13
import           Commands.Frontends.Dragon13.Shim
import           Commands.Test.Arbitrary()

import qualified Test.QuickCheck.Property          as Q

import Control.Exception (toException) 


-- | if the grammar has already been escaped, its serialization into the shim must be valid Python.
prop_DNSGrammar :: DNSGrammar () DNSText DNSName -> Q.Result
prop_DNSGrammar grammar = case applyShim getShim config (serializeGrammar grammar) of
 Left  e  -> Q.exception "prop_DNSGrammar" (toException e) 
 Right {} -> Q.succeeded
-- rejected = result { ok = Nothing }  i.e.  Discard  i.e.  as if precondition were False
 where
 config = NatLinkConfig address "E:/commands/log.txt" "E:/commands/context.json"
 address = Address (Host "localhost") (Port 8000)

