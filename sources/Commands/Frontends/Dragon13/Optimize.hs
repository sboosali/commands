{-# LANGUAGE DataKinds, LambdaCase, ViewPatterns #-}
module Commands.Frontends.Dragon13.Optimize where
import           Commands.Etc                      ()
import           Commands.Frontends.Dragon13.Lens
import           Commands.Frontends.Dragon13.Types
import           Commands.Grammar.Types

import           Control.Lens
import           Control.Lens.Plated               ()
import           Data.Bifunctor                    (first)
import           Data.List
import qualified Data.List                         as List
import           Data.List.NonEmpty                (NonEmpty (..))
import           Data.Monoid                       ((<>))
import           Data.Text.Lazy                    (Text)
import           Numeric.Natural
-- import qualified Data.Text.Lazy      as T


-- |
type DNSCycle n t = [(SomeDNSLHS (DNSMetaName n), DNSProduction True (DNSMetaName n) t)]

-- TODO downcastDNSProduction

-- | transforms a 'DNSGrammar' into one that Dragon NaturallySpeaking accepts:
--
-- * grammars with recursive productions crash Dragon
-- NaturallySpeaking (no cycle detection I guess?)
-- * grammars that are "too complex" (by some opaque metric) throw a
-- @BadGrammar@ exception:
-- inlining decreases depth by increasing breadth
--
-- TODO prop> introduces no naming collisions
--
optimizeGrammar :: DNSGrammar (DNSMetaName LHS) t -> DNSGrammar Text t
optimizeGrammar
 = first renderDNSMetaName
 . vocabulariseGrammar
 . inlineGrammar
 . expandGrammar
 . compactGrammar

-- |
renderDNSMetaName :: DNSMetaName Text -> Text
renderDNSMetaName = view dnsMetaName  -- TODO use dnsMetaExpansion


-- |
compactGrammar :: DNSGrammar (DNSMetaName LHS) t -> DNSGrammar (DNSMetaName Text) t
compactGrammar = undefined


-- |
expandGrammar :: (Eq n) => DNSGrammar (DNSMetaName n) t -> DNSGrammar (DNSMetaName n) t
expandGrammar = id

{- | expands each recursive 'DNSProduction' in the cycle (i.e. the input)
to the depth 'dnsExpand'.

the original name is preserved as the "root", to be consistent with
references outside the cycle.


-- TODO nonempty

-- TODO prop> length (expandProductionCycle ps) == expandProductionCycle_measure ps

-- TODO Arbitrary newtype must be biased towards mutually recursive productions

-}
expandProductionCycle :: DNSCycle Text t -> [DNSProduction True (DNSMetaName Text) t]
expandProductionCycle ps = concatMap (expandProductionCycleTo ps (expandProductionMaxDepth ps)) (fmap snd ps)

-- |
--
-- concrete 'Text', not abstract @n@, to use 'emptyDNSRHS'.
expandProductionCycleTo :: DNSCycle Text t -> Natural -> DNSProduction True (DNSMetaName Text) t -> [DNSProduction True (DNSMetaName Text) t]
expandProductionCycleTo ps d p@(DNSProduction l r)
  = [DNSProduction l $ fmap (expandRHSAt ps d) r]
 <> fmap (\k -> expandProductionAt ps k p) [1..d]
 <> [DNSProduction (expandLHSAt 0 l) $ first defaultDNSMetaName emptyDNSRHS :| []]

expandProductionAt :: (Eq n) => DNSCycle n t -> Natural -> DNSProduction True (DNSMetaName n) t -> DNSProduction True (DNSMetaName n) t
expandProductionAt ps d (DNSProduction l r) = DNSProduction (expandLHSAt d l) $ fmap (expandRHSAt ps (d-1)) r

expandRHSAt :: (Eq n) => DNSCycle n t -> Natural -> DNSRHS (DNSMetaName n) t -> DNSRHS (DNSMetaName n) t
expandRHSAt ps d = transform $ \case
 DNSNonTerminal (shouldExpand ps -> Just (SomeDNSLHS l)) -> DNSNonTerminal (SomeDNSLHS (expandLHSAt d l))
 r -> r

shouldExpand :: (Eq n) => DNSCycle n t -> SomeDNSLHS (DNSMetaName n) -> Maybe (SomeDNSLHS (DNSMetaName n))
shouldExpand ps l1 = case List.find ((==l1).fst) ps of
 Just (l2,_) -> Just l2
 _ -> Nothing


expandLHSAt :: Natural -> DNSLHS l (DNSMetaName n) -> DNSLHS l (DNSMetaName n)
expandLHSAt = set (dnsLHSName.dnsMetaExpansion._Just)



{- |

-- TODO prop> \(NonEmpty ps) -> length ps <= expandProductionCycle_measure ps

-}
expandProductionCycle_measure :: DNSCycle n t -> Natural
expandProductionCycle_measure ps = n + (n * d)
 where
 n = genericLength ps
 d = expandProductionMaxDepth ps

{- | for a cycle of mutually-recursive productions, take the 'maximum'
of their 'dnsExpand's.

since we must expand each production to the
same depth, and since each production can be configured with its own
depth, we need some aggregate.


-}
expandProductionMaxDepth :: DNSCycle n t -> Natural
expandProductionMaxDepth
 = maximum
 . fmap (maybe 0 id)  -- each Nothing comes from DNSBuiltin
 . fmap (\p -> p ^? dnsProductionExpand)
 . fmap snd
 where
 dnsProductionExpand = dnsProductionName.dnsMetaInfo.dnsExpand



-- |
inlineGrammar :: (Eq n) => DNSGrammar (DNSMetaName n) t -> DNSGrammar (DNSMetaName n) t
inlineGrammar = id



-- |
vocabulariseGrammar :: (Eq n) => DNSGrammar (DNSMetaName n) t -> DNSGrammar (DNSMetaName n) t
vocabulariseGrammar = id


