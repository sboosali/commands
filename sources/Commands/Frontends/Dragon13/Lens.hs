{-# LANGUAGE DataKinds, GADTs, LambdaCase, TemplateHaskell #-}
module Commands.Frontends.Dragon13.Lens where
import Commands.Etc
import Commands.Frontends.Dragon13.Types
import Commands.Symbol.Types

import Control.Lens
import Data.Tree.Lens

import Data.Function                     (on)
import Data.Maybe                        (mapMaybe)
import Data.Tree
import Numeric.Natural


makeLenses ''DNSGrammar
makeLenses ''DNSVocabulary
makeLenses ''DNSProduction
makePrisms ''DNSRHS

makeLenses ''DNSReifying
makeLenses ''DNSExpandedName
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

ruleExpand :: Lens' (Rule p DNSReifying l i a) Natural
ruleExpand = ruleDNSProduction.dnsProductionInfo.dnsExpand

ruleInline :: Lens' (Rule p DNSReifying l i a) Bool
ruleInline = ruleDNSProduction.dnsProductionInfo.dnsInline

-- | a lens into the root production that represents a rule.
ruleDNSProduction :: Lens' (Rule p DNSReifying l i a) (DNSReifyingProduction l i)
ruleDNSProduction = ruleReified.dnsReifyingDescendents.root

-- | a lens into a tree of the transitive dependencies of the production.
ruleDNSDescendents :: Lens' (Rule p DNSReifying l i a) [Tree (DNSReifyingProduction l i)]
ruleDNSDescendents = ruleReified.dnsReifyingDescendents.branches
