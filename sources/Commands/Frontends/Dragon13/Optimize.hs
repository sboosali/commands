{-# LANGUAGE DataKinds, GADTs, LambdaCase, ViewPatterns #-}
module Commands.Frontends.Dragon13.Optimize where
import           Commands.Extra
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
import qualified Data.List                         as List
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Numeric.Natural


-- | a grammar can be normalized/optimized with its @i@ and @n@ type parameters.
type DNSGrammarOptimizeable t n = DNSGrammar DNSInfo t n

-- | see 'simplifyGrammar'.
type DNSProductionOptimizeable t n = DNSProduction DNSInfo t n

-- | see 'vocabularizeGrammar'
type DNSVocabularyOptimizeable t n = DNSVocabulary DNSInfo t n

-- | see ''. an edge in an adjacency graph.
type DNSAdjacency t n = Adjacency (SomeDNSLHS n) (DNSProductionOptimizeable t n)

-- | see 'inlineGrammar'. quick access to the right-hand side of a production to be inlined.
type DNSInlined t n = Map (SomeDNSLHS n) (DNSRHS t n)

-- | see 'vocabularizeGrammar'
type DNSVocabularized n = Map n (DNSLHS LHSList LHSDefined n)


-- ================================================================ --

{-| transforms a 'DNSGrammar' into one that Dragon NaturallySpeaking accepts:

* grammars with recursive productions crash Dragon
NaturallySpeaking (no cycle detection I guess?)
* grammars that are "too complex" (by some opaque metric) throw a
@BadGrammar@ exception:
inlining decreases depth by increasing breadth

TODO prop> introduces no naming collisions

-}
optimizeGrammar' :: (Eq t) => DnsOptimizationSettings -> DNSGrammar DNSInfo t LHS -> DNSGrammar DNSInfo t Text
optimizeGrammar' settings
 = tidyupGrammar
 -- . expandGrammar
 . vocabularizeGrammar
 . inlineGrammar settings
 . simplifyGrammar

optimizeDNSInfoGrammar :: (Eq t, Ord n) => DnsOptimizationSettings -> DNSGrammar DNSInfo t n -> DNSGrammar DNSInfo t n
optimizeDNSInfoGrammar settings
 = vocabularizeGrammar
 . inlineGrammar settings
 . simplifyGrammar

optimizeAnyDNSGrammar :: (Eq t, Ord n) => DnsOptimizationSettings -> DNSGrammar i t n -> DNSGrammar i t n
optimizeAnyDNSGrammar _settings
 = vocabularizeGrammar
 . simplifyGrammar


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

{-| inline any "small" productions.

-}
inlineGrammar :: (Ord n) => DnsOptimizationSettings -> DNSGrammar DNSInfo t n -> DNSGrammar DNSInfo t n
-- inlineGrammar = inlineGrammar'
inlineGrammar settings grammar = inlineGrammar' grammar'
 where
 grammar' = if settings^.dnsOptimizeInlineSmall
  then (grammar & over (dnsProductions.each) markInlinedIfSmall)
  else grammar
-- NOTE markInlinedIfSmall doesn't need to be iterated, as inlining only increases the size of a right-hand side

{-| we don't inline away 'DNSVocabulary's because they:

* have simple names, for easy debugging.
* are "cheap" wrt Dragon NaturallySpeaking's opaque grammar-complexity measure.

the 'dnsExport' is never inlined away.

-}
inlineGrammar' :: (Ord n) => DNSGrammar DNSInfo t n -> DNSGrammar DNSInfo t n
inlineGrammar' (DNSGrammar (_e:|_ps) _vs _is) = DNSGrammar (e:|ps) _vs _is
 where
 e = inlineProduction theInlined $ _e
 ps = inlineProduction theInlined <$> notInlined
 theInlined = yesInlined -- TODO  rewriteOn each other?
 (yesInlined, notInlined) = partitionInlined _ps

-- -- | assumes the 'DNSInlined' are acyclic wrt each other: otherwise, doesn't terminate.
-- inlineAway :: (Ord n) => DNSInlined t n -> [DNSProduction DNSInfo t n] -> [DNSProduction DNSInfo t n]
-- inlineAway = undefined -- TODO  rewriteOn?

inlineProduction :: (Ord n) => DNSInlined t n -> DNSProduction DNSInfo t n -> DNSProduction DNSInfo t n
inlineProduction lrs = rewriteOn dnsProductionRHS $ \case
 DNSNonTerminal l -> shouldInlineLHS lrs l
 _ -> Nothing

shouldInlineLHS :: (Ord n) => DNSInlined t n -> SomeDNSLHS n -> Maybe (DNSRHS t n)
shouldInlineLHS lrs l = Map.lookup l lrs

partitionInlined
 :: (Ord n)
 => [DNSProduction DNSInfo t n]
 -> ( DNSInlined t n , [DNSProduction DNSInfo t n] )
partitionInlined ps = (yesInlined, notInlined)  -- TODO don't in-line cycles
 where
 yesInlined = Map.fromList . fmap (\(DNSProduction _ l r) -> (SomeDNSLHS l, r)) $ _yesInlined
 (_yesInlined, notInlined) = List.partition (view (dnsProductionInfo.dnsInline)) ps


markInlinedIfSmall :: DNSProduction DNSInfo t n -> DNSProduction DNSInfo t n
markInlinedIfSmall (DNSProduction i l r) = DNSProduction i' l r
 where
 i' = if isDNSRHSSmall r
  then i & set dnsInline True
  else i

{-| any right-hand side without non-singleton 'DNSSequence' or 'DNSAlternatives' is small, even nested

-}
isDNSRHSSmall :: DNSRHS t n -> Bool
isDNSRHSSmall = \case
 DNSTerminal     {} -> True
 DNSNonTerminal  {} -> True
 DNSOptional     r  -> isDNSRHSSmall r
 DNSMultiple     r  -> isDNSRHSSmall r
 DNSSequence     (r :| []) -> isDNSRHSSmall r
 DNSAlternatives (r :| []) -> isDNSRHSSmall r
 _ -> False



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
tidyupGrammar :: DNSGrammar i t LHS -> DNSGrammar i t Text
tidyupGrammar = second (T.pack . showLHS . tidyupLHS)

-- |
tidyupLHS :: LHS -> LHS
tidyupLHS = bimapLHS fGUI fInt
 where
 fGUI (GUI (Package _) (Module _) (Identifier occ)) = GUI (Package "") (Module "") (Identifier occ)
 fInt = id
 -- TODO safely tidyup i.e. unambiguously. compare each against all, with getNames. Build a Trie?


-- ================================================================ --

simplifyGrammar :: (Eq t, Eq n) => DNSGrammar i t n -> DNSGrammar i t n
simplifyGrammar = over (dnsProductions.each.dnsProductionRHS) (simplifyRHS)

{- |

all simplifications are "inductive" (I think that's the word), i.e. they structurally reduce the input. Thus, we know 'rewrite' terminates.

-}
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
 (_,ys) -> Just ys
