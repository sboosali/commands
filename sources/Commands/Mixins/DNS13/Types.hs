{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}
-- | provides concrete types that can be "mixed in" to 'Grammar'.
module Commands.Mixins.DNS13.Types where
import Commands.Frontends.Dragon13.Lens
import Commands.Frontends.Dragon13.Types
import Commands.Grammar.Types

import Control.Lens

import Numeric.Natural


type DNSReifying = DNSReifyingProduction
-- DNSGrammar DNSInfo DNSReifyingName DNSReifyingToken

type DNSReifyingProduction = DNSProduction DNSInfo DNSReifyingName DNSReifyingToken

defaultDNSReifyingProduction :: LHS -> DNSReifyingRHS -> DNSReifyingProduction
defaultDNSReifyingProduction l r = DNSProduction defaultDNSInfo (DNSRule (defaultDNSExpandedName l)) r

type DNSReifyingRHS = DNSRHS DNSReifyingName DNSReifyingToken

type DNSReifyingName = DNSExpandedName LHS

-- | not 'Text' because user-facing "config" modules (e.g.
-- "Reifyings.Plugins.Example") can only use *one* of:
--
-- * OverloadedStrings
-- * type class sugar
--
-- as we need to trigger a class @String@ instantiates.
-- TODO can we make this work with defaulting?
type DNSReifyingToken = String


-- | a name, with the level of its expansion.
--
-- '_dnsExpansion' tracks which level a recursive 'DNSProduction' has been expanded to.
--
-- when the '_dnsExpansion' is @Nothing@, the 'DNSProduction' will not be expanded.
--
-- when the '_dnsExpansion' is @Just 0@, the 'DNSProduction' will only
-- hold base case 'DNSAlternative's, not the recursive 'DNSAlternative's.
--
data DNSExpandedName n = DNSExpandedName
 { _dnsExpansion    :: Maybe Natural
 , _dnsExpandedName :: n
 }
 deriving (Show,Eq,Ord,Functor)

-- | yet un-expanded
defaultDNSExpandedName :: n -> DNSExpandedName n
defaultDNSExpandedName = DNSExpandedName Nothing

-- | metadata to properly transform a 'DNSGrammar' into one that Dragon NaturallySpeaking accepts.
--
--
data DNSInfo = DNSInfo
 { _dnsExpand :: !Natural -- ^ how many times to expand a recursive 'DNSProduction'
 , _dnsInline :: !Bool    -- ^ whether or not to inline a 'DNSProduction'
 }
 deriving (Show,Eq,Ord)

-- | no expansion and no inlining.
defaultDNSInfo :: DNSInfo
defaultDNSInfo = DNSInfo 0 False


makeLenses ''DNSExpandedName
makeLenses ''DNSInfo

gramExpand :: Lens' (Grammar p DNSReifying a) Natural
gramExpand = gramGrammar.dnsProductionInfo.dnsExpand

gramInline :: Lens' (Grammar p DNSReifying a) Bool
gramInline = gramGrammar.dnsProductionInfo.dnsInline

