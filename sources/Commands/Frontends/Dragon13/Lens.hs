{-# LANGUAGE DataKinds, GADTs, LambdaCase, TemplateHaskell #-}
module Commands.Frontends.Dragon13.Lens where
import Commands.Etc
import Commands.Frontends.Dragon13.Types

import Control.Applicative
import Control.Lens
import Data.Function                     (on)
import Data.Maybe                        (mapMaybe)


makeLenses ''DNSGrammar
makeLenses ''DNSVocabulary
makeLenses ''DNSProduction
makePrisms ''DNSRHS


-- | equality projected 'on' the left-hand sides of productions.
equalDNSProduction :: (Eq n) => DNSProduction i n t -> DNSProduction i n t -> Bool
equalDNSProduction = (==) `on` view dnsProductionLHS

-- |
getNonTerminals :: DNSProduction i n t -> [SomeDNSLHS n]
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


dnsExport :: Lens' (DNSGrammar i n t) (DNSProduction i n t)
dnsExport = dnsProductions . nonemptyHead

dnsNonExports :: Lens' (DNSGrammar i n t) [DNSProduction i n t]
dnsNonExports = dnsProductions . nonemptyTail
