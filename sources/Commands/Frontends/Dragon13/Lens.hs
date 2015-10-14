{-# LANGUAGE DataKinds, GADTs, LambdaCase, TemplateHaskell #-}
module Commands.Frontends.Dragon13.Lens where
import Commands.Extra
import Commands.Frontends.Dragon13.Types

import Control.Lens

import Data.Function                     (on)
import Data.Maybe                        (mapMaybe)


makeLenses ''DNSGrammar
makeLenses ''DNSVocabulary
makeLenses ''DNSProduction
makePrisms ''DNSRHS

makeLenses ''DNSInfo

-- | equality projected 'on' the left-hand sides of productions.
equalDNSProduction :: (Eq n) => DNSProduction i t n -> DNSProduction i t n -> Bool
equalDNSProduction = (==) `on` view dnsProductionLHS

-- |
getNonTerminals :: DNSProduction i t n -> [SomeDNSLHS n]
getNonTerminals
 = mapMaybe (\case
    DNSNonTerminal l -> Just l
    _                -> Nothing)
 . universeOn dnsProductionRHS

dnsSomeLHSName :: Traversal' (SomeDNSLHS n) n
dnsSomeLHSName f (SomeDNSLHS l) = SomeDNSLHS <$> dnsLHSName f l

-- not a Prism because it merges two cases
dnsLHSName :: Traversal' (DNSLHS l s n) n
dnsLHSName f = \case
 DNSRule n   -> DNSRule <$> f n
 DNSList n   -> DNSList <$> f n
 l           -> pure l

dnsExport :: Lens' (DNSGrammar i t n) (DNSProduction i t n)
dnsExport = dnsProductions . nonemptyHead

dnsNonExports :: Lens' (DNSGrammar i t n) [DNSProduction i t n]
dnsNonExports = dnsProductions . nonemptyTail
