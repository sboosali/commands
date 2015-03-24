{-# LANGUAGE DataKinds, GADTs, LambdaCase, TemplateHaskell #-}
module Commands.Frontends.Dragon13.Lens where
import Commands.Frontends.Dragon13.Types
import Commands.Etc

import Control.Applicative
import Control.Lens
import Data.Function                     (on)
import Data.Maybe                        (mapMaybe)


makeLenses ''DNSGrammar
makeLenses ''DNSVocabulary
makeLenses ''DNSProduction
makePrisms ''DNSRHS

-- |
getNonTerminals :: DNSProduction i n t -> [SomeDNSLHS n]
getNonTerminals
 = mapMaybe (\case
    DNSNonTerminal l -> Just l
    _                -> Nothing)
 . universeOn dnsProductionRHS

-- | equality projected 'on' the left-hand sides of productions.
equalDNSProduction :: (Eq n) => DNSProduction i n t -> DNSProduction i n t -> Bool
equalDNSProduction = (==) `on` view dnsProductionLHS

dnsSomeLHSName :: Traversal' (SomeDNSLHS n) n
dnsSomeLHSName f (SomeDNSLHS l) = SomeDNSLHS <$> dnsLHSName f l

-- not a Prism because it merges two cases
dnsLHSName :: Traversal' (DNSLHS l n) n
dnsLHSName f = \case
 DNSRule n   -> DNSRule <$> f n
 DNSList n   -> DNSList <$> f n
 l           -> pure l

dnsExport :: Lens' (DNSGrammar i n t) (DNSProduction i n t)
dnsExport = dnsProductions . nonemptyHead
