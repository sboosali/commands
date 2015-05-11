{-# LANGUAGE GADTs, LambdaCase, ScopedTypeVariables #-}
module Commands.Frontends.Dragon13.Induce where
import Commands.Frontends.Dragon13.Types
import Commands.Mixins.DNS13.Types
import Commands.Symbol.Types

import Data.List.NonEmpty                (NonEmpty (..), nonEmpty)


{- |

generates:

* nested binary 'DNSSequence's
* extra 'unitDNSRHS's
* extra 'zeroDNSRHS's

which should be collapsed/pruned away.

-}
induceDNSProduction
 :: l
 -> RHS p r l i x               -- ^ note: @r@ is polymorphic, thus ignored
 -> DNSProduction DNSInfo l i
induceDNSProduction lhs = DNSProduction defaultDNSInfo (DNSRule lhs) . foldRHS
 fromSymbol
 unitDNSRHS
 (\x y -> DNSSequence (x :| [y]))
 (maybe zeroDNSRHS DNSAlternatives . nonEmpty)

 where
 fromSymbol :: Symbol (Rule p r l) i x -> DNSRHS l i
 fromSymbol = \case
  Terminal    i             -> DNSTerminal    (DNSToken i)
  NonTerminal (Rule l _ _)  -> DNSNonTerminal (SomeDNSLHS (DNSRule l))
  Opt         x             -> DNSOptional                 (fromSymbol x)
  Many        x             -> (DNSOptional . DNSMultiple) (fromSymbol x)
  Some        x             -> DNSMultiple                 (fromSymbol x)
