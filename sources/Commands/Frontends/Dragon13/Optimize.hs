{-# LANGUAGE DataKinds, GADTs, LambdaCase, ViewPatterns #-}
module Commands.Frontends.Dragon13.Optimize where
import           Commands.Etc
import           Commands.Frontends.Dragon13.Lens
import           Commands.Frontends.Dragon13.Types
import           Commands.Grammar
import           Commands.Grammar.Types
import           Commands.Graph

import           Control.Applicative
import           Control.Lens
import           Data.Bifunctor                    (first)
import           Data.Either                       (partitionEithers)
import           Data.Foldable                     (foldMap)
import           Data.Graph
import qualified Data.List                         as List
import           Data.List.NonEmpty                (NonEmpty (..))
import qualified Data.List.NonEmpty                as NonEmpty
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Monoid                       ((<>))
import           Data.Semigroup.Applicative        (Ap (..))
import           Data.Text.Lazy                    (Text)
import qualified Data.Text.Lazy                    as T
import           Numeric.Natural


-- |
type DNSGrammarOptimizeable n t = DNSGrammar DNSInfo (DNSExpandedName n) t

-- |
type DNSProductionOptimizeable n t = DNSProduction DNSInfo (DNSExpandedName n) t

type DNSVocabularyOptimizeable n t = DNSVocabulary DNSInfo (DNSExpandedName n) t

-- |
type DNSAdjacency n t = Adjacency (SomeDNSLHS (DNSExpandedName n)) (DNSProductionOptimizeable n t)

-- |
type DNSExpanded n t = [SomeDNSLHS (DNSExpandedName n)]

-- |
type DNSInlined n t = Map (SomeDNSLHS (DNSExpandedName n)) (DNSRHS (DNSExpandedName n) t)

-- |
type DNSVocabularized n = Map (DNSExpandedName n) (DNSLHS LHSList (DNSExpandedName n))


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
optimizeGrammar :: (Eq t) => DNSGrammar DNSInfo (DNSExpandedName LHS) t -> DNSGrammar DNSInfo Text t
optimizeGrammar
 = first renderDNSExpandedName
 . tidyupGrammar
 . expandGrammar
 . vocabularizeGrammar
 . inlineGrammar
 . simplifyGrammar



-- ================================================================ --

{- |

eliminates all cycles from the graph induced by 'dnsAdjacency':

TODO prop> all (\case { AcyclicSCC{} -> True; _ -> False }) ('stronglyConnComp' . fmap 'dnsAdjacency' . toListOf dnsProductions . expandGrammar $ g)

preserves the 'dnsExport':

TODO prop> (g ^. dnsExport) `equalDNSProduction` (expandGrammar g ^. dnsExport)

-}
expandGrammar :: (Eq n, Ord n) => DNSGrammarOptimizeable n t -> DNSGrammarOptimizeable n t
expandGrammar (DNSGrammar (_e:|_ps) _vs _is) = DNSGrammar (e:|ps) _vs _is
 where
 ([e], ps) = List.partition (`equalDNSProduction` _e) expanded -- TODO should pattern match fail, expansion has corrupted the grammar
 expanded = expandSCCs . stronglyConnComp . fmap dnsAdjacency $ (_e:_ps)

{- | induces a graph on a 'DNSGrammar', where:

* nodes are 'DNSProduction's
* out-edges are when a 'DNSProduction's 'DNSRHS' holds a 'DNSProduction's 'DNSLHS' as a 'DNSNonTerminal'.

-}
dnsAdjacency :: DNSProductionOptimizeable n t -> DNSAdjacency n t
dnsAdjacency p = (p,l,ls)
 where
 l  = SomeDNSLHS (p^.dnsProductionLHS)
 ls = getNonTerminals p

-- |
--
expandSCCs :: (Eq n) => [SCC (DNSProductionOptimizeable n t)] -> [DNSProductionOptimizeable n t]
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
expandProductionCycle :: (Eq n) => [DNSProductionOptimizeable n t] -> [DNSProductionOptimizeable n t]
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
 -> DNSProductionOptimizeable n t
 -> [DNSProductionOptimizeable n t]
expandProductionCycleTo ls d p@(DNSProduction i l r)
  = [DNSProduction i l $ expandRHSAt ls d r]
 <> fmap (\k -> expandProductionAt ls k p) (reverse [1..d])
 <> [DNSProduction i (expandLHSAt 0 l) $ defaultDNSExpandedName `first` zeroDNSRHS]

expandProductionAt :: (Eq n) => DNSExpanded n t -> Natural -> DNSProductionOptimizeable n t -> DNSProductionOptimizeable n t
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
expandProductionCycle_measure ::  [DNSProductionOptimizeable n t] -> Natural
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
expandProductionMaxDepth :: [DNSProductionOptimizeable n t] -> Natural
expandProductionMaxDepth
 = maximum
 . fmap (\p -> p ^. dnsProductionInfo.dnsExpand)


-- ================================================================ --

-- | we don't inline away 'DNSVocabulary's because they:
--
-- * have simple names, for easy debugging.
-- * are "cheap" or even "free", wrt Dragon NaturallySpeaking's opaque grammar-complexity measure.
--
-- the 'dnsExport' is never inlined away.
--
--
inlineGrammar :: (Ord n) => DNSGrammarOptimizeable n t -> DNSGrammarOptimizeable n t
inlineGrammar (DNSGrammar (_e:|_ps) _vs _is) = DNSGrammar (e:|ps) _vs _is
 where
 e = inlineProduction theInlined $ _e
 ps = inlineProduction theInlined <$> notInlined
 theInlined = yesInlined -- TODO  rewriteOn each other?
 (yesInlined, notInlined) = partitionInlined _ps

-- | assumes the 'DNSInlined' are acyclic wrt each other: otherwise, doesn't terminate.
inlineAway :: (Ord n) => DNSInlined n t -> [DNSProductionOptimizeable n t] -> [DNSProductionOptimizeable n t]
inlineAway = undefined -- TODO  rewriteOn?

inlineProduction :: (Ord n) => DNSInlined n t -> DNSProductionOptimizeable n t -> DNSProductionOptimizeable n t
inlineProduction lrs = rewriteOn dnsProductionRHS $ \case
 DNSNonTerminal l -> shouldInline lrs l
 _ -> Nothing

shouldInline :: (Ord n) => DNSInlined n t -> SomeDNSLHS (DNSExpandedName n) -> Maybe (DNSRHS (DNSExpandedName n) t)
shouldInline lrs l = Map.lookup l lrs

partitionInlined
 :: (Ord n)
 => [DNSProductionOptimizeable n t]
 -> ( DNSInlined n t , [DNSProductionOptimizeable n t] )
partitionInlined ps = (yesInlined, notInlined)  -- TODO don't in-line cycles
 where
 yesInlined = Map.fromList . fmap (\(DNSProduction _ l r) -> (SomeDNSLHS l, r)) $ _yesInlined
 (_yesInlined, notInlined) = List.partition (view (dnsProductionInfo.dnsInline)) ps



-- ================================================================ --

-- |
-- TODO prop>
vocabularizeGrammar :: (Ord n, Eq t) => DNSGrammarOptimizeable n t -> DNSGrammarOptimizeable n t
vocabularizeGrammar (DNSGrammar _ps _vs _is) = DNSGrammar ps vs _is
 where
 ps = rules2lists vocabularized <$> productions
 vs = vocabularies <> _vs
 vocabularized = Map.fromList . fmap (\(DNSList n) -> (n, DNSList n)) $ (vocabularies ^.. each.dnsVocabularyLHS) -- TODO partial function
 (productions, vocabularies) = partitionVocabularizables _ps

-- |
partitionVocabularizables
 :: NonEmpty (DNSProductionOptimizeable n t)
 -> (NonEmpty (DNSProductionOptimizeable n t), [DNSVocabularyOptimizeable n t])
partitionVocabularizables (e:|_ps) = (e:|ps, vs)
 where
 (ps, vs) = partitionEithers . fmap canVocabularize $ _ps

-- |
canVocabularize :: DNSProductionOptimizeable n t -> Either (DNSProductionOptimizeable n t) (DNSVocabularyOptimizeable n t)
canVocabularize p@(DNSProduction i (DNSRule n) r) = case (getApp . onlyTokens) r of
 Nothing -> Left p
 Just ts -> Right $ DNSVocabulary i (DNSList n) ts
canVocabularize _ = undefined -- TODO distinguish between LHS that can and can't be on the left-hand side, as it were

{- | returns all the tokens in the right-hand side, but only if that
right-hand side has only tokens (or nested alternatives thereof).

>>> :set -XOverloadedLists -XOverloadedStrings

>>> getApp $ onlyTokens $ DNSTerminal "one"
Just [DNSToken "one"]

>>> getApp $ onlyTokens $ DNSSequence ["one"]
Nothing

>>> getApp $ onlyTokens $ DNSAlternatives ["one", DNSNonTerminal undefined]
Nothing

>>> getApp $ onlyTokens $ DNSAlternatives ["one", DNSAlternatives ["two", "three"], "four"]
Just [DNSToken "one",DNSToken "two",DNSToken "three",DNSToken "four"]

@('foldMap' onlyTokens :: [DNSRHS n t] -> 'Ap' Maybe [t])@,
where the 'Foldable' is @[]@ and the 'Monoid' is @'Ap' Maybe [_]@,
has the correct short-circuiting behavior.
(see <https://byorgey.wordpress.com/2011/04/18/monoids-for-maybe/>)


-}
onlyTokens :: DNSRHS n t -> Ap Maybe [DNSToken t]
onlyTokens = \case
 DNSTerminal t      -> (Ap . Just) [t]
 DNSAlternatives rs -> foldMap onlyTokens rs
 _                  -> Ap Nothing

-- |
rules2lists :: (Ord n) => DNSVocabularized n -> DNSProductionOptimizeable n t -> DNSProductionOptimizeable n t
rules2lists ls = transformOn dnsProductionRHS $ \case
 DNSNonTerminal (SomeDNSLHS (DNSRule ((flip Map.lookup) ls -> Just l))) -> DNSNonTerminal (SomeDNSLHS l)
 r -> r



-- ================================================================ --

-- | Tidy up the grammar by contacting the left-hand sides, without collisions.
tidyupGrammar :: (Functor f) => DNSGrammar i (f LHS) t -> DNSGrammar i (f Text) t
tidyupGrammar = first (fmap (T.pack . showLHS . tidyupLHS))
-- tidyupGrammar = first (fmap (T.pack . showLHS))

-- |
tidyupLHS :: LHS -> LHS
tidyupLHS (LHS (GUI (Package _) (Module _) (Identifier occ))) = LHS (GUI (Package "") (Module "") (Identifier occ))
tidyupLHS (l `LHSApp` ls) = tidyupLHS l `LHSApp` fmap tidyupLHS ls
tidyupLHS l = l
-- TODO safely tidyup i.e. unambiguously. compare each against all, with getNames. Build a Trie?

-- |
renderDNSExpandedName :: DNSExpandedName Text -> Text
renderDNSExpandedName (DNSExpandedName Nothing  n) = n
renderDNSExpandedName (DNSExpandedName (Just k) n) = n <> T.pack "____" <> T.pack (show k)


-- ================================================================ --

simplifyGrammar :: (Eq t, Eq n) => DNSGrammar i n t -> DNSGrammar i n t
simplifyGrammar = over (dnsProductions.each.dnsProductionRHS) simplifyRHS

-- | all simplifications are "inductive" (I think that's the word), i.e. they structurally reduce the input. Thus, we know 'rewrite' terminates.
simplifyRHS :: (Eq t, Eq n) => DNSRHS n t -> DNSRHS n t
simplifyRHS = rewrite $ \case
 DNSSequence     (r :| [])   -> Just r
 DNSAlternatives (r :| [])   -> Just r
 DNSMultiple (DNSMultiple r) -> Just $ DNSMultiple r -- TODO  valid?
 DNSOptional (DNSOptional r) -> Just $ DNSOptional r -- TODO  valid?
 -- terminates:
 DNSAlternatives (NonEmpty.partition (==zeroDNSRHS) -> ((_:_), rs)) ->
   Just $ maybe zeroDNSRHS DNSAlternatives (NonEmpty.nonEmpty rs)
 _ -> Nothing
