{-# LANGUAGE GADTs, LambdaCase, ScopedTypeVariables #-}
module Commands.Frontends.Dragon13.Induce where
import Commands.Frontends.Dragon13.Types
import Commands.Symbol.Types

import Data.List.NonEmpty                (NonEmpty (..), nonEmpty)

import Data.Tree



-- deriveDNSGrammar :: Grammar p DNSReifying l i a -> DNSGrammar DNSInfo l i
-- deriveDNSGrammar
--  = fmap (DNSGrammar (getDescendentProductions rule) [] dnsHeader
--  . maybe to list
--  . map getDescendentProductions
--  . map (\(Rule l r _) -> induceDNSReified l r)
--  . flattenGrammar

-- -- | a production and its transitive dependencies. removes duplicates. doesn't detect cycles.
-- getDescendentProductions :: (Eq l) => Rule p DNSReifying l i x -> NonEmpty (DNSReifyingProduction l i)
-- getDescendentProductions rule = NonEmpty.nubBy equalDNSProduction (export :| nonExports)
--  where
--  export = (rule^.ruleDNSProduction)
--  nonExports = flatten =<< (rule^.ruleDNSDescendents)

{- |

the core glue between `commands-core` and `commands-frontends-dragon13`.

-}
induceDNSReified :: l -> RHS p DNSReifying l i a -> DNSReifying l i
induceDNSReified lhs rhs = DNSReifying $ Node
 (defaultDNSProduction lhs (induceDNSRHS rhs))
 (rhs2productions rhs)

{- | "induce"s a serializable right-hand side, from a left-hand side and a right-hand side.

generates:

* nested binary 'DNSSequence's
* extra 'UnitDNSRHS's
* extra 'ZeroDNSRHS's ('RHS's instantiate @Alternative@, and so may be @empty@. but 'DNSProduction's take @NonEmpty (DNSRHS n t)@.)

which should be collapsed/pruned away.

-}
induceDNSRHS
 :: RHS p r l i x               -- ^ note: @r@ is polymorphic, thus ignored
 -> DNSReifyingRHS l i
induceDNSRHS = foldRHS
 induceDNSSymbol
 UnitDNSRHS
 (\x y -> DNSSequence (x :| [y]))
 (maybe ZeroDNSRHS DNSAlternatives . nonEmpty)
 DNSOptional
 (DNSOptional . DNSMultiple)
 DNSMultiple

induceDNSSymbol :: Symbol (Rule p r l) i x -> DNSReifyingRHS l i
induceDNSSymbol = \case
 Terminal    i             -> DNSTerminal    (DNSToken i)
 NonTerminal (Rule l _ _)  -> DNSNonTerminal (SomeDNSLHS (DNSRule (defaultDNSExpandedName l)))

rhs2productions :: RHS p DNSReifying l i x -> [Tree (DNSReifyingProduction l i)]
-- TODO rhs2productions rhs = rhs2rules rhs & to Map.elems . each . _Any . ruleReified . _Wrapped
rhs2productions = foldRHS_Monoid $ \case
  Terminal                              _ -> []
  NonTerminal (Rule _ (DNSReifying ps) _) -> [ps]

