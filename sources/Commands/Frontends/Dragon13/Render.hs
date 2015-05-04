{-# LANGUAGE DataKinds, ExistentialQuantification, LambdaCase #-}
module Commands.Frontends.Dragon13.Render where -- TODO rename it to Induce
import Commands.Frontends.Dragon13.Lens
import Commands.Frontends.Dragon13.Types
import Commands.Grammar.Types
import Commands.Mixins.DNS13.Types
import Control.Alternative.Free.Associated

import Control.Lens                        (view)
import Data.List.NonEmpty                  (nonEmpty)

import Data.Maybe                          (catMaybes, fromMaybe, mapMaybe)


-- | "induce"s a production from a left-hand side and a right-hand side, which can then be "serialize"d into text.
--
-- 'RHS's instantiate @Alternative@, and so may be @empty@. but 'DNSProduction's take @NonEmpty (DNSRHS n t)@. we must use Dragon's @{emptyList}@ (i.e. 'zeroDNSRHS') for empty 'RHS's (later optimized away).
induceDNSProduction :: Rule p DNSReifying x -> DNSProduction DNSInfo DNSReifyingName DNSReifyingToken
induceDNSProduction (Rule l r) = DNSProduction defaultDNSInfo lhs rhs
 where
 lhs = (DNSRule . defaultDNSExpandedName) l
 rhs = (fromMaybe zeroDNSRHS . induceDNSProduction_RHS) r

-- |
induceDNSProduction_RHS :: RHS p DNSReifying x -> Maybe (DNSRHS DNSReifyingName DNSReifyingToken)
induceDNSProduction_RHS = \case
 Pure _     -> Nothing
 Many rs    -> DNSAlternatives <$> (nonEmpty . mapMaybe  induceDNSProduction_RHS $ rs)
 fs `App` x -> DNSSequence     <$> (nonEmpty . catMaybes $ [induceDNSProduction_RHS fs, Just (induceDNSProduction_Symbol x)])
 fs :<*> xs -> DNSSequence     <$> (nonEmpty . catMaybes $ [induceDNSProduction_RHS fs, induceDNSProduction_RHS xs])
-- TODO define as sound generic free alternative operation, with an (Applicative) Const DNSRHS?

-- |
induceDNSProduction_Symbol :: Symbol p DNSReifying x -> DNSRHS DNSReifyingName DNSReifyingToken
induceDNSProduction_Symbol = symbol
 (DNSTerminal    . DNSToken)
 (DNSNonTerminal . SomeDNSLHS . view (gramGrammar.dnsProductionLHS))
