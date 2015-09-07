{-# LANGUAGE DataKinds, GADTs, LambdaCase, ViewPatterns #-}
module Commands.Frontends.Dragon13.Optimize where
import           Commands.Etc
import           Commands.Frontends.Dragon13.Lens
import           Commands.Frontends.Dragon13.Types
import           Commands.LHS

import           Control.Lens
import           Data.List.NonEmpty                (NonEmpty (..))
import qualified Data.List.NonEmpty                as NonEmpty
import           Data.Semigroup.Applicative        (Ap (..))
import           Data.Text.Lazy                    (Text)
import qualified Data.Text.Lazy                    as T

import           Data.Bifunctor                    (second)
import           Data.Either                       (partitionEithers)
import           Data.Graph
import qualified Data.List                         as List
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Monoid                       ((<>))
import           Numeric.Natural


-- | a grammar can be normalized/optimized with its @i@ and @n@ type parameters.
type DNSGrammarOptimizeable t n = DNSGrammar DNSInfo t (DNSExpandedName n)

-- | see 'simplifyGrammar'.
type DNSProductionOptimizeable t n = DNSProduction DNSInfo t (DNSExpandedName n)

-- | see 'vocabularizeGrammar'
type DNSVocabularyOptimizeable t n = DNSVocabulary DNSInfo t (DNSExpandedName n)

-- | see ''. an edge in an adjacency graph.
type DNSAdjacency t n = Adjacency (SomeDNSLHS (DNSExpandedName n)) (DNSProductionOptimizeable t n)

-- | see 'expandGrammar'
type DNSExpanded n = [SomeDNSLHS (DNSExpandedName n)]

-- | see 'inlineGrammar'. quick access to the right-hand side of a production to be inlined.
type DNSInlined t n = Map (SomeDNSLHS n) (DNSRHS t n)

-- | see 'vocabularizeGrammar'
type DNSVocabularized n = Map n (DNSLHS LHSList LHSDefined n)


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
optimizeGrammar' :: (Eq t) => DNSGrammar DNSInfo t (DNSExpandedName LHS) -> DNSGrammar DNSInfo t Text
optimizeGrammar'
 = second renderDNSExpandedName
 . tidyupGrammar
 . expandGrammar
 . vocabularizeGrammar
 . inlineGrammar
 . simplifyGrammar

optimizeDNSInfoGrammar :: (Eq t, Ord n) => DNSGrammar DNSInfo t n -> DNSGrammar DNSInfo t n
optimizeDNSInfoGrammar
 = vocabularizeGrammar
 . inlineGrammar
 . simplifyGrammar

optimizeAnyDNSGrammar :: (Eq t, Ord n) => DNSGrammar i t n -> DNSGrammar i t n
optimizeAnyDNSGrammar
 = vocabularizeGrammar
 . simplifyGrammar


-- ================================================================ --

{- |

eliminates all cycles from the graph induced by 'dnsAdjacency':

TODO prop> all (\case { AcyclicSCC{} -> True; _ -> False }) ('stronglyConnComp' . fmap 'dnsAdjacency' . toListOf dnsProductions . expandGrammar $ g)

preserves the 'dnsExport':

TODO prop> (g ^. dnsExport) `equalDNSProduction` (expandGrammar g ^. dnsExport)

-}
expandGrammar :: (Eq n, Ord n) => DNSGrammarOptimizeable t n -> DNSGrammarOptimizeable t n
expandGrammar (DNSGrammar (_e:|_ps) _vs _is) = DNSGrammar (e:|ps) _vs _is
 where
 ([e], ps) = List.partition (`equalDNSProduction` _e) expanded -- TODO should pattern match fail, expansion has corrupted the grammar
 expanded = expandSCCs . stronglyConnComp . fmap dnsAdjacency $ (_e:_ps)

{- | induces a graph on a 'DNSGrammar', where:

* nodes are 'DNSProduction's
* out-edges are when a 'DNSProduction's 'DNSRHS' holds a 'DNSProduction's 'DNSLHS' as a 'DNSNonTerminal'.

-}
dnsAdjacency :: DNSProductionOptimizeable t n -> DNSAdjacency t n
dnsAdjacency p = (p,l,ls)
 where
 l  = SomeDNSLHS (p^.dnsProductionLHS)
 ls = getNonTerminals p

-- |
--
expandSCCs :: (Eq n) => [SCC (DNSProductionOptimizeable t n)] -> [DNSProductionOptimizeable t n]
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
expandProductionCycle :: (Eq n) => [DNSProductionOptimizeable t n] -> [DNSProductionOptimizeable t n]
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
 => DNSExpanded n
 -> Natural
 -> DNSProductionOptimizeable t n
 -> [DNSProductionOptimizeable t n]
expandProductionCycleTo ls d p@(DNSProduction i l r)
  = [DNSProduction i l $ expandRHSAt ls d r]
 <> fmap (\k -> expandProductionAt ls k p) (reverse [1..d])
 <> [DNSProduction i (expandLHSAt 0 l) $ defaultDNSExpandedName `second` ZeroDNSRHS]

expandProductionAt :: (Eq n) => DNSExpanded n -> Natural -> DNSProductionOptimizeable t n -> DNSProductionOptimizeable t n
expandProductionAt ls d (DNSProduction i l r) = DNSProduction i (expandLHSAt d l) (expandRHSAt ls (d-1) r)

expandLHSAt :: Natural -> DNSLHS l s (DNSExpandedName n) -> DNSLHS l s (DNSExpandedName n)
expandLHSAt = set (dnsLHSName.dnsExpansion) . Just

expandRHSAt :: (Eq n) => DNSExpanded n -> Natural -> DNSRHS t (DNSExpandedName n) -> DNSRHS t (DNSExpandedName n)
expandRHSAt ls d = transform $ \case
 DNSNonTerminal (shouldExpand ls -> Just (SomeDNSLHS l)) -> DNSNonTerminal (SomeDNSLHS (expandLHSAt d l))
 r -> r

shouldExpand :: (Eq n) => DNSExpanded n -> SomeDNSLHS (DNSExpandedName n) -> Maybe (SomeDNSLHS (DNSExpandedName n))
shouldExpand ls l = List.find (==l) ls



{- |

-- TODO prop> \(NonEmpty ls) -> length ls <= expandProductionCycle_measure ls

-}
expandProductionCycle_measure ::  [DNSProductionOptimizeable t n] -> Natural
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
expandProductionMaxDepth :: [DNSProductionOptimizeable t n] -> Natural
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
inlineGrammar :: (Ord n) => DNSGrammar DNSInfo t n -> DNSGrammar DNSInfo t n
inlineGrammar (DNSGrammar (_e:|_ps) _vs _is) = DNSGrammar (e:|ps) _vs _is
 where
 e = inlineProduction theInlined $ _e
 ps = inlineProduction theInlined <$> notInlined
 theInlined = yesInlined -- TODO  rewriteOn each other?
 (yesInlined, notInlined) = partitionInlined _ps

-- | assumes the 'DNSInlined' are acyclic wrt each other: otherwise, doesn't terminate.
inlineAway :: (Ord n) => DNSInlined t n -> [DNSProduction DNSInfo t n] -> [DNSProduction DNSInfo t n]
inlineAway = undefined -- TODO  rewriteOn?

inlineProduction :: (Ord n) => DNSInlined t n -> DNSProduction DNSInfo t n -> DNSProduction DNSInfo t n
inlineProduction lrs = rewriteOn dnsProductionRHS $ \case
 DNSNonTerminal l -> shouldInline lrs l
 _ -> Nothing

shouldInline :: (Ord n) => DNSInlined t n -> SomeDNSLHS n -> Maybe (DNSRHS t n)
shouldInline lrs l = Map.lookup l lrs

partitionInlined
 :: (Ord n)
 => [DNSProduction DNSInfo t n]
 -> ( DNSInlined t n , [DNSProduction DNSInfo t n] )
partitionInlined ps = (yesInlined, notInlined)  -- TODO don't in-line cycles
 where
 yesInlined = Map.fromList . fmap (\(DNSProduction _ l r) -> (SomeDNSLHS l, r)) $ _yesInlined
 (_yesInlined, notInlined) = List.partition (view (dnsProductionInfo.dnsInline)) ps



-- ================================================================ --

-- |
-- TODO prop>
vocabularizeGrammar :: (Ord n, Eq t) => DNSGrammar i t n -> DNSGrammar i t n
vocabularizeGrammar (DNSGrammar _ps _vs _is) = DNSGrammar ps vs _is
 where
 ps = rules2lists vocabularized <$> productions
 vs = vocabularies <> _vs
 vocabularized = Map.fromList . fmap (\(DNSList n) -> (n, DNSList n)) $ (vocabularies ^.. each.dnsVocabularyLHS) -- TODO partial function
 (productions, vocabularies) = partitionVocabularizables _ps

-- |
partitionVocabularizables
 :: NonEmpty (DNSProduction i t n)
 -> (NonEmpty (DNSProduction i t n), [DNSVocabulary i t n])
partitionVocabularizables (e:|_ps) = (e:|ps, vs)
 where
 (ps, vs) = partitionEithers . fmap canVocabularize $ _ps

-- |
canVocabularize :: DNSProduction i t n -> Either (DNSProduction i t n) (DNSVocabulary i t n)
canVocabularize p@(DNSProduction i (DNSRule n) r) = case (getAp . onlyTokens) r of
 Nothing -> Left p
 Just ts -> Right $ DNSVocabulary i (DNSList n) ts  -- make sure this doesn't introduce a naming conflict with existing DNSList. might be best to just treat rules and lists as if they shared the same namespace.

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

@('foldMap' onlyTokens :: [DNSRHS t n] -> 'Ap' Maybe [t])@,
where the 'Foldable' is @[]@ and the 'Monoid' is @'Ap' Maybe [_]@,
has the correct short-circuiting behavior.
(see <https://byorgey.wordpress.com/2011/04/18/monoids-for-maybe/>)


-}
onlyTokens :: DNSRHS t n -> Ap Maybe [DNSToken t]
onlyTokens = \case
 DNSTerminal t      -> (Ap . Just) [t]
 DNSAlternatives rs -> foldMap onlyTokens rs
 _                  -> Ap Nothing

-- |
rules2lists :: (Ord n) => DNSVocabularized n -> DNSProduction i t n -> DNSProduction i t n
rules2lists ls = transformOn dnsProductionRHS $ \case
 DNSNonTerminal (SomeDNSLHS (DNSRule ((flip Map.lookup) ls -> Just l))) -> DNSNonTerminal (SomeDNSLHS l)
 r -> r



-- ================================================================ --

-- | Tidy up the grammar by contacting the left-hand sides, without collisions.
tidyupGrammar :: (Functor f) => DNSGrammar i t (f LHS) -> DNSGrammar i t (f Text)
tidyupGrammar = second (fmap (T.pack . showLHS . tidyupLHS))

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

simplifyGrammar :: (Eq t, Eq n) => DNSGrammar i t n -> DNSGrammar i t n
simplifyGrammar = over (dnsProductions.each.dnsProductionRHS) simplifyRHS

-- | all simplifications are "inductive" (I think that's the word), i.e. they structurally reduce the input. Thus, we know 'rewrite' terminates.
simplifyRHS :: (Eq t, Eq n) => DNSRHS t n -> DNSRHS t n
simplifyRHS = rewrite $ \case
 -- singleton
 DNSSequence     (r :| [])   -> Just r
 DNSAlternatives (r :| [])   -> Just r
 -- idempotent
 DNSMultiple (DNSMultiple r) -> Just$ DNSMultiple r -- TODO  valid?
 DNSOptional (DNSOptional r) -> Just$ DNSOptional r -- TODO  valid?
 -- additive identity. terminates.
 DNSAlternatives (filterAway (==ZeroDNSRHS) -> Just [])     -> Just$ ZeroDNSRHS
 DNSAlternatives (filterAway (==ZeroDNSRHS) -> Just (r:rs)) -> Just$ DNSAlternatives (r :| rs)
 -- multiplicative identity. terminates.
 DNSSequence     (filterAway (==UnitDNSRHS) -> Just [])     -> Just$ UnitDNSRHS
 DNSSequence     (filterAway (==UnitDNSRHS) -> Just (r:rs)) -> Just$ DNSSequence     (r :| rs)
 --
 _ -> Nothing

{- |

@
filter (not p) <$> filterAway p xs = filterAway p xs
filterAway p xs = Nothing | Just (filter (not p) xs)

(<) (length <$> filterAway p xs) (Just (length xs))
-- Nothing | Just True

fromJust (filterAway p xs) < (length xs)
True

"inductive"
case filterAway p xs of
 Nothing -> True
 Just ys  -> length ys < length xs

case filterAway (==x) xs of
 Nothing -> not (x elem xs)
 Just ys  -> not (x elem ys)

@

@
case filterAway p xs of
 Nothing -> -- nothing was filtered away ("input preserved")
 Just [] -> -- everything was filtered away
 Just (x:xs) -> -- some items were filtered away
@

-}
filterAway :: (a -> Bool) -> NonEmpty a -> Maybe [a]
filterAway p xs = case NonEmpty.partition p xs of -- partition odd [1,2,3,4] == ([1,3], [2,4])
 ([],_) -> Nothing
 (_,xs) -> Just xs

