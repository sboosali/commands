{-# LANGUAGE ViewPatterns #-}
module Commands.Test.Properties where
import Commands.Etc                      hiding (failed)
import Commands.Frontends.Dragon13
import Commands.Frontends.Dragon13.Text
import Commands.Frontends.Dragon13.Types
import Commands.Test.Arbitrary
import Commands.Test.Types
import Data.Bifunctor                    (bimap)
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Property


-- | if the grammar can be escaped, its serialization must be valid Python.
prop_DNSGrammar :: DNSGrammar () DNSName DNSText -> Result
prop_DNSGrammar grammar = case isPythonFile . display . serializeGrammar $ grammar of
  Left _  -> failed
  Right _ -> succeeded

-- rejected = result { ok = Nothing }  i.e.  Discard  i.e.  as if precondition were False
