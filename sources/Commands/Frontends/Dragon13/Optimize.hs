{-# LANGUAGE DataKinds, LambdaCase, NamedFieldPuns, ViewPatterns #-}
module Commands.Frontends.Dragon13.Optimize where
import           Commands.Etc
import           Commands.Frontends.Dragon13.Lens
import           Commands.Frontends.Dragon13.Types
import           Commands.Grammar
import           Commands.Grammar.Types
import           Commands.Graph

import           Control.Lens
import           Data.Bifunctor                    (first)
import           Data.Graph
import qualified Data.List                         as List
import           Data.Map.Strict                   (Map)
-- import qualified Data.Map.Strict                   as Map
import           Control.Applicative
import           Data.List.NonEmpty                (NonEmpty (..))
import           Data.Monoid                       ((<>))
import           Data.Text.Lazy                    (Text)
import qualified Data.Text.Lazy                    as T
import           Numeric.Natural


-- |
type DNSAdjacency n t = Adjacency (SomeDNSLHS (DNSExpandedName n)) (DNSProduction DNSInfo (DNSExpandedName n) t)

-- |
type DNSExpanded n t = [SomeDNSLHS (DNSExpandedName n)]

-- |
type DNSInlined n t = Map (SomeDNSLHS (DNSExpandedName n)) (DNSRHS (DNSExpandedName n) t)



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
optimizeGrammar :: DNSGrammar DNSInfo (DNSExpandedName LHS) t -> DNSGrammar DNSInfo Text t
optimizeGrammar
 = first renderDNSExpandedName
 . compactGrammar
 -- . vocabulariseGrammar
 . expandGrammar
 -- . inlineGrammar -- TODO



-- ================================================================ --

{- |

eliminates all cycles from the graph induced by 'dnsAdjacency':

TODO prop> all (\case { AcyclicSCC{} -> True; _ -> False }) ('stronglyConnComp' . fmap 'dnsAdjacency' . toListOf dnsProductions . expandGrammar $ g)

preserves the 'dnsExport':

TODO prop> (g ^. dnsExport) `equalDNSProduction` (expandGrammar g ^. dnsExport)

-}
expandGrammar :: (Eq n, Ord n) => DNSGrammar DNSInfo (DNSExpandedName n) t -> DNSGrammar DNSInfo (DNSExpandedName n) t
expandGrammar (DNSGrammar (_e:|_ps) _vs _is) = DNSGrammar (e:|ps) _vs _is
 where
 ([e], ps) = List.partition (`equalDNSProduction` _e) expanded -- TODO if pattern match fails, expansion has corrupted the grammar
 expanded = expandSCCs . stronglyConnComp . fmap dnsAdjacency $ (_e:_ps)

{- | induces a graph on a 'DNSGrammar', where:

* nodes are 'DNSProduction's
* out-edges are when a 'DNSProduction's 'DNSRHS' holds a 'DNSProduction's 'DNSLHS' as a 'DNSNonTerminal'.

-}
dnsAdjacency :: DNSProduction DNSInfo (DNSExpandedName n) t -> DNSAdjacency n t
dnsAdjacency p = (p,l,ls)
 where
 l  = SomeDNSLHS (p^.dnsProductionLHS)
 ls = getNonTerminals p

-- |
--
expandSCCs :: (Eq n) => [SCC (DNSProduction DNSInfo (DNSExpandedName n) t)] -> [DNSProduction DNSInfo (DNSExpandedName n) t]
expandSCCs = concatMap $ \case
 AcyclicSCC p -> [p] -- acyclic productions don't need to be expanded
 CyclicSCC ps -> expandProductionCycle ps

{- | expands each recursive 'DNSProduction' in the cycle (i.e. the input)
to the depth 'dnsExpand'.

the original name is preserved as the "root", to be consistent with
references outside the cycle.


-- TODO nonempty

-- TODO? prop> length (expandProductionCycle c) == expandProductionCycle_measure c

-- TODO Arbitrary newtype should be biased towards mutually recursive productions, maybe by parameterising on @n@ a small enum.

-}
expandProductionCycle :: (Eq n) => [DNSProduction DNSInfo (DNSExpandedName n) t] -> [DNSProduction DNSInfo (DNSExpandedName n) t]
expandProductionCycle ps = expandProductionCycleTo ls (expandProductionMaxDepth ps) =<< ps
 where
 ls = SomeDNSLHS <$> (ps ^.. each.dnsProductionLHS)

-- |
--
-- guarantees that the 'DNSLHS' of the input 'DNSProduction', matches the 'DNSLHS' of the first output 'DNSProduction'.
--
-- something like prop> \e n p -> let (q:|_) = expandProductionCycleTo e n p in q `equalDNSProduction` p
--
-- TODO this guarantee then guarantees the success of the irrefutable pattern match in expandGrammar i.e. ([e], ps)
expandProductionCycleTo
 :: (Eq n)
 => DNSExpanded n t
 -> Natural
 -> DNSProduction DNSInfo (DNSExpandedName n) t
 -> [DNSProduction DNSInfo (DNSExpandedName n) t]
expandProductionCycleTo ls d p@(DNSProduction i l r)
  = [DNSProduction i l $ expandRHSAt ls d r]
 <> fmap (\k -> expandProductionAt ls k p) (reverse [1..d])
 <> [DNSProduction i (expandLHSAt 0 l) $ defaultDNSExpandedName `first` zeroDNSRHS]

expandProductionAt :: (Eq n) => DNSExpanded n t -> Natural -> DNSProduction DNSInfo (DNSExpandedName n) t -> DNSProduction DNSInfo (DNSExpandedName n) t
expandProductionAt ls d (DNSProduction i l r) = DNSProduction i (expandLHSAt d l) (expandRHSAt ls (d-1) r)

expandLHSAt :: Natural -> DNSLHS l (DNSExpandedName n) -> DNSLHS l (DNSExpandedName n)
expandLHSAt = set (dnsLHSName.dnsExpansion) . Just

expandRHSAt :: (Eq n) => DNSExpanded n t -> Natural -> DNSRHS (DNSExpandedName n) t -> DNSRHS (DNSExpandedName n) t
expandRHSAt ls d = transform $ \case
 DNSNonTerminal (shouldExpand ls -> Just (SomeDNSLHS l)) -> DNSNonTerminal (SomeDNSLHS (expandLHSAt d l))
 r -> r

shouldExpand :: (Eq n) => DNSExpanded n t -> SomeDNSLHS (DNSExpandedName n) -> Maybe (SomeDNSLHS (DNSExpandedName n))
shouldExpand ls l = List.find (==l) ls



{- |

-- TODO prop> \(NonEmpty ls) -> length ls <= expandProductionCycle_measure ls

-}
expandProductionCycle_measure ::  [DNSProduction DNSInfo (DNSExpandedName n) t] -> Natural
expandProductionCycle_measure ps = n + (n * d)
 where
 n = List.genericLength ps
 d = expandProductionMaxDepth ps

{- | for a cycle of mutually-recursive productions, take the 'maximum'
of their 'dnsExpand's.

since we must expand each production to the
same depth, and since each production can be configured with its own
depth, we need some aggregate.


-}
expandProductionMaxDepth :: [DNSProduction DNSInfo (DNSExpandedName n) t] -> Natural
expandProductionMaxDepth
 = maximum
 . fmap (\p -> p ^. dnsProductionInfo.dnsExpand)


-- -- ================================================================ --

-- -- | we don't in-line 'DNSVocabulary's because they:
-- --
-- -- * have simple names, for easy debugging.
-- -- * are "cheap", wrt Dragon NaturallySpeaking's opaque grammar-complexity metric.
-- --
-- --
-- inlineGrammar :: (Ord n) => DNSGrammar DNSInfo (DNSExpandedName n) t -> DNSGrammar DNSInfo (DNSExpandedName n) t
-- inlineGrammar g@DNSGrammar{_dnsExport,_dnsImports,_dnsProductions}
--  = DNSGrammar e _dnsImports (vocabularies <> fmap upcastDNSProduction ps)
--  where
--  ([e], ps) = partition (`equalDNSProduction` _dnsExport) isInlined -- TODO if pattern match fails, inlining has corrupted the grammar
--  isInlined = fmap (inlineProduction theInlined) notInlined -- TODO  rewriteOn?
--  (notInlined, theInlined) = partitionInlined productions
--  (vocabularies, productions) = partitionDNSGrammar g

-- inlineAway :: (Ord n) => DNSInlined n t -> [DNSProduction DNSInfo (DNSExpandedName n) t] -> [DNSProduction DNSInfo (DNSExpandedName n) t]
-- inlineAway = undefined -- TODO  rewriteOn?

-- -- TODO  rewriteOn because:
-- inlineProduction :: (Ord n) => DNSInlined n t -> DNSProduction DNSInfo (DNSExpandedName n) t -> DNSProduction DNSInfo (DNSExpandedName n) t
-- inlineProduction lrs = transformOn dnsProductionRHS $ \case
--  DNSNonTerminal (shouldInline lrs -> Just r) -> r
--  r -> r

-- shouldInline :: (Ord n) => DNSInlined n t -> SomeDNSLHS (DNSExpandedName n) -> Maybe (DNSRHS (DNSExpandedName n) t)
-- shouldInline lrs l = Map.lookup l lrs

-- toBeInlined
--  :: DNSProduction DNSInfo (DNSExpandedName n) t
--  -> Either (DNSProduction DNSInfo (DNSExpandedName n) t) (DNSProduction DNSInfo (DNSExpandedName n) t)
-- toBeInlined p = case p ^? (dnsProductionName.dnsMetaInfo.dnsInline) of
--  Just True -> Right p
--  _         -> Left p

-- -- TODO maybe inline export into productions, but never inline it away
-- partitionInlined :: (Ord n) => [DNSProduction DNSInfo (DNSExpandedName n) t] -> ([DNSProduction DNSInfo (DNSExpandedName n) t], DNSInlined n t)
-- partitionInlined ps = (notInlined, theInlined)
--  where
--  theInlined = Map.fromList . fmap (\(DNSProduction l r) -> (SomeDNSLHS l, r)) $ yesInlined
--  (notInlined, yesInlined) = partitionEithers . fmap toBeInlined $ ps



-- -- ================================================================ --

-- -- |
-- vocabulariseGrammar :: (Eq n, Eq t) => DNSGrammar DNSInfo (DNSExpandedName n) t -> DNSGrammar DNSInfo (DNSExpandedName n) t
-- vocabulariseGrammar = id





-- -- ================================================================ --

-- |
compactGrammar :: (Functor f) => DNSGrammar i (f LHS) t -> DNSGrammar i (f Text) t
compactGrammar = first (fmap (T.pack . showLHS . compactLHS))
-- compactGrammar = first (fmap (T.pack . showLHS))

-- |
compactLHS :: LHS -> LHS
compactLHS (LHS (GUI (Package _) (Module _) (Identifier occ))) = LHS (GUI (Package "") (Module "") (Identifier occ))
compactLHS (l `LHSApp` ls) = compactLHS l `LHSApp` fmap compactLHS ls
compactLHS l = l
-- TODO safely compact i.e. unambiguously. compare each against all, with getNames. Build a Trie?

-- |
renderDNSExpandedName :: DNSExpandedName Text -> Text
renderDNSExpandedName (DNSExpandedName Nothing  n) = n
renderDNSExpandedName (DNSExpandedName (Just k) n) = n <> T.pack "____" <> T.pack (show k)


-- ================================================================ --
