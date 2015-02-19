{-# LANGUAGE ViewPatterns #-}
module Commands.Test.Properties where
import Commands.Etc hiding  (failed)
import Commands.Test.Arbitrary
import Commands.Test.Types
import           Commands.Frontends.Dragon13
import           Commands.Frontends.Dragon13.Text
import Commands.Frontends.Dragon13.Types
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers
import Data.Bifunctor (bimap)


-- | if the grammar can be escaped, its serialization must be valid Python.
prop_DNSGrammar :: DNSGrammar DNSName DNSText -> Result
prop_DNSGrammar grammar = case isPythonFile . display . serializeGrammar $ grammar of
  Left _  -> failed
  Right _ -> succeeded

-- rejected = result { ok = Nothing }  i.e.  Discard  i.e.  as if precondition were False
