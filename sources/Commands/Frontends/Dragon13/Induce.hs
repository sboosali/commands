{-# LANGUAGE GADTs, LambdaCase, ScopedTypeVariables #-}
module Commands.Frontends.Dragon13.Induce where
import Commands.Frontends.Dragon13.Types
import Commands.Mixins.DNS13.Types
import Commands.Symbol.Types

import Data.List.NonEmpty                (NonEmpty (..), nonEmpty)


{- |

generates:

* nested binary 'DNSSequence's
* extra 'UnitDNSRHS's
* extra 'ZeroDNSRHS's

which should be collapsed/pruned away.

-}
induceDNSProduction
 :: l
 -> RHS p r l i x               -- ^ note: @r@ is polymorphic, thus ignored
 -> DNSProduction DNSInfo l i
induceDNSProduction lhs = DNSProduction defaultDNSInfo (DNSRule lhs) . foldRHS
 induceDNSSymbol
 UnitDNSRHS
 (\x y -> DNSSequence (x :| [y]))
 (maybe ZeroDNSRHS DNSAlternatives . nonEmpty)
 DNSOptional
 (DNSOptional . DNSMultiple)
 DNSMultiple

induceDNSSymbol :: Symbol (Rule p r l) i x -> DNSRHS l i
induceDNSSymbol = \case
 Terminal    i             -> DNSTerminal    (DNSToken i)
 NonTerminal (Rule l _ _)  -> DNSNonTerminal (SomeDNSLHS (DNSRule l))
