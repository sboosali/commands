{-# LANGUAGE DataKinds, LambdaCase, NamedFieldPuns, ViewPatterns #-}
module Commands.Frontends.Dragon13.Optimize where
import           Commands.Etc
import           Commands.Frontends.Dragon13.Lens
import           Commands.Frontends.Dragon13.Types
import           Commands.Grammar
import           Commands.Grammar.Types
import           Commands.Graph

import           Control.Lens
import           Control.Lens.Plated               ()
import           Data.Bifunctor                    (first)
import           Data.Either                       (partitionEithers)
import           Data.Graph
import           Data.List
import qualified Data.List                         as List
import           Data.List.NonEmpty                (NonEmpty (..))
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Monoid                       ((<>))
import           Data.Text.Lazy                    (Text)
import qualified Data.Text.Lazy                    as T
import           Numeric.Natural


-- |
type DNSAdjacency n t = Adjacency (SomeDNSLHS (DNSMetaName n)) (DNSProduction True (DNSMetaName n) t)

-- |
type DNSExpanded n t = [SomeDNSLHS (DNSMetaName n)]

-- |
type DNSInlined n t = Map (SomeDNSLHS (DNSMetaName n)) (DNSRHS (DNSMetaName n) t)



-- ================================================================ --

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
 . compactGrammar
 . vocabulariseGrammar
 . inlineGrammar
 -- . expandGrammar -- TODO



-- ================================================================ --

-- |
expandGrammar :: (Eq n, Ord n) => DNSGrammar (DNSMetaName n) t -> DNSGrammar (DNSMetaName n) t
expandGrammar g@DNSGrammar{_dnsExport,_dnsImports}
 = DNSGrammar e _dnsImports (vocabularies <> fmap upcastDNSProduction ps)
 where
 ([e], ps) = partition (`equalDNSProduction` _dnsExport) expanded -- TODO if pattern match fails, expansion has corrupted the grammar
 expanded = expandSCCs . stronglyConnComp . fmap dnsAdjacency $ productions
 (vocabularies,productions) = partitionDNSGrammar g

dnsAdjacency :: DNSProduction True (DNSMetaName n) t -> DNSAdjacency n t
dnsAdjacency p = (p,l,ls)
 where
 l  = p ^. dnsProductionLHS
 ls = getNonTerminals p

-- |
--
expandSCCs :: (Eq n) => [SCC (DNSProduction True (DNSMetaName n) t)] -> [DNSProduction True (DNSMetaName n) t]
expandSCCs = concatMap $ \case
 AcyclicSCC p -> [p]
 CyclicSCC ps -> expandProductionCycle ps

{- | expands each recursive 'DNSProduction' in the cycle (i.e. the input)
to the depth 'dnsExpand'.

the original name is preserved as the "root", to be consistent with
references outside the cycle.


-- TODO nonempty

-- TODO? prop> length (expandProductionCycle c) == expandProductionCycle_measure c

-- TODO Arbitrary newtype must be biased towards mutually recursive productions

-}
expandProductionCycle :: (Eq n) => [DNSProduction True (DNSMetaName n) t] -> [DNSProduction True (DNSMetaName n) t]
expandProductionCycle ps = concatMap (expandProductionCycleTo ls (expandProductionMaxDepth ps)) ps
 where
 ls = ps ^.. (each.dnsProductionLHS)

-- |
--
expandProductionCycleTo :: (Eq n) => DNSExpanded n t -> Natural -> DNSProduction True (DNSMetaName n) t -> [DNSProduction True (DNSMetaName n) t]
expandProductionCycleTo ls d p@(DNSProduction l r)
  = [DNSProduction l $ fmap (expandRHSAt ls d) r] -- TODO this guarantees the irrefutable pattern match above i.e. ([e], ps)
 <> fmap (\k -> expandProductionAt ls k p) [1..d]
 <> [DNSProduction (expandLHSAt 0 l) $ first defaultDNSMetaName zeroDNSRHS :| []]

expandProductionAt :: (Eq n) => DNSExpanded n t -> Natural -> DNSProduction True (DNSMetaName n) t -> DNSProduction True (DNSMetaName n) t
expandProductionAt ls d (DNSProduction l r) = DNSProduction (expandLHSAt d l) $ fmap (expandRHSAt ls (d-1)) r

expandRHSAt :: (Eq n) => DNSExpanded n t -> Natural -> DNSRHS (DNSMetaName n) t -> DNSRHS (DNSMetaName n) t
expandRHSAt ls d = transform $ \case
 DNSNonTerminal (shouldExpand ls -> Just (SomeDNSLHS l)) -> DNSNonTerminal (SomeDNSLHS (expandLHSAt d l))
 r -> r

shouldExpand :: (Eq n) => DNSExpanded n t -> SomeDNSLHS (DNSMetaName n) -> Maybe (SomeDNSLHS (DNSMetaName n))
shouldExpand ls l = List.find (==l) ls

expandLHSAt :: Natural -> DNSLHS l (DNSMetaName n) -> DNSLHS l (DNSMetaName n)
expandLHSAt = set (dnsLHSName.dnsMetaExpansion._Just)



{- |

-- TODO prop> \(NonEmpty ls) -> length ls <= expandProductionCycle_measure ls

-}
expandProductionCycle_measure ::  [DNSProduction True (DNSMetaName n) t] -> Natural
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
expandProductionMaxDepth :: [DNSProduction True (DNSMetaName n) t] -> Natural
expandProductionMaxDepth
 = maximum
 . fmap (maybe 0 id)  -- each Nothing comes from DNSBuiltin
 . fmap (\p -> p ^? dnsProductionExpand)
 where
 dnsProductionExpand = dnsProductionName.dnsMetaInfo.dnsExpand



-- ================================================================ --

-- | we don't in-line 'DNSVocabulary's because they:
--
-- * have simple names, for easy debugging.
-- * are "cheap", wrt Dragon NaturallySpeaking's opaque grammar-complexity metric.
--
--
inlineGrammar :: (Ord n) => DNSGrammar (DNSMetaName n) t -> DNSGrammar (DNSMetaName n) t
inlineGrammar g@DNSGrammar{_dnsExport,_dnsImports,_dnsProductions}
 = DNSGrammar e _dnsImports (vocabularies <> fmap upcastDNSProduction ps)
 where
 ([e], ps) = partition (`equalDNSProduction` _dnsExport) isInlined -- TODO if pattern match fails, inlining has corrupted the grammar
 isInlined = fmap (inlineProduction theInlined) notInlined -- TODO  rewriteOn?
 (notInlined, theInlined) = partitionInlined productions
 (vocabularies, productions) = partitionDNSGrammar g

inlineAway :: (Ord n) => DNSInlined n t -> [DNSProduction True (DNSMetaName n) t] -> [DNSProduction True (DNSMetaName n) t]
inlineAway = undefined -- TODO  rewriteOn?

-- TODO  rewriteOn because:
inlineProduction :: (Ord n) => DNSInlined n t -> DNSProduction True (DNSMetaName n) t -> DNSProduction True (DNSMetaName n) t
inlineProduction lrs = transformOn dnsProductionRHS $ \case
 DNSNonTerminal (shouldInline lrs -> Just r) -> r
 r -> r

shouldInline :: (Ord n) => DNSInlined n t -> SomeDNSLHS (DNSMetaName n) -> Maybe (DNSRHS (DNSMetaName n) t)
shouldInline lrs l = Map.lookup l lrs

toBeInlined
 :: DNSProduction True (DNSMetaName n) t
 -> Either (DNSProduction True (DNSMetaName n) t) (DNSProduction True (DNSMetaName n) t)
toBeInlined p = case p ^? (dnsProductionName.dnsMetaInfo.dnsInline) of
 Just True -> Right p
 _         -> Left p

-- TODO maybe inline export into productions, but never inline it away
partitionInlined :: (Ord n) => [DNSProduction True (DNSMetaName n) t] -> ([DNSProduction True (DNSMetaName n) t], DNSInlined n t)
partitionInlined ps = (notInlined, theInlined)
 where
 theInlined = Map.fromList . fmap (\(DNSProduction l rs) -> (SomeDNSLHS l, DNSAlternatives rs)) $ yesInlined
 (notInlined, yesInlined) = partitionEithers . fmap toBeInlined $ ps



-- ================================================================ --

-- |
vocabulariseGrammar :: (Eq n) => DNSGrammar (DNSMetaName n) t -> DNSGrammar (DNSMetaName n) t
vocabulariseGrammar = id


-- ================================================================ --

-- |
renderDNSMetaName :: DNSMetaName Text -> Text
renderDNSMetaName = view dnsMetaName
-- TODO use dnsMetaExpansion

-- |
compactGrammar :: DNSGrammar (DNSMetaName LHS) t -> DNSGrammar (DNSMetaName Text) t
compactGrammar = first (fmap (T.pack . showLHS . compactLHS))

-- |
compactLHS :: LHS -> LHS
compactLHS (LHS (GUI (Package _) (Module _) (Identifier occ))) = LHS (GUI (Package "") (Module "") (Identifier occ))
compactLHS (l `LHSApp` ls) = compactLHS l `LHSApp` fmap compactLHS ls
compactLHS l = l
-- TODO safely compact i.e. unambiguously. compare each against all, with getNames. Build a Trie


-- ================================================================ --
