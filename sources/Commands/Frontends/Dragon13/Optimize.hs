{-# LANGUAGE ViewPatterns #-}
module Commands.Frontends.Dragon13.Optimize where
import Commands.Etc                      ()
import Commands.Frontends.Dragon13.Types
import Commands.Grammar.Types

import Control.Lens
import Data.List
import Numeric.Natural


-- | transforms a 'DNSGrammar' into one that Dragon NaturallySpeaking accepts:
--
-- * grammars with recursive productions crash Dragon
-- NaturallySpeaking (no cycle detection I guess?)
-- * grammars that are "too complex" (by some opaque metric) throw a
-- @BadGrammar@ exception:
-- inlining decreases depth by increasing breadth
--
-- TODO property: introduces no naming collisions
optimizeGrammar :: DNSGrammar DNSMetaName t -> DNSGrammar DNSMetaName t
optimizeGrammar = compactGrammar . vocabulariseGrammar . inlineGrammar . expandGrammar


expandGrammar :: DNSGrammar DNSMetaName t -> DNSGrammar DNSMetaName t
expandGrammar = id

{- | expands each recursive 'DNSProduction' in the cycle (i.e. the input)
to the depth 'dnsExpand'.

the original name is preserved as the "root", to be consistent with
references outside the cycle.


-- TODO nonempty

-- TODO prop> length (expandProductionCycle ps) == expandProductionCycle_measure ps

-- TODO Arbitrary newtype must be biased towards mutually recursive productions

-}
expandProductionCycle :: [DNSProduction e DNSMetaName t] -> [DNSProduction e DNSMetaName t]
expandProductionCycle ps = undefined d
 where
 d = expandProductionDepth ps


{- |

-- TODO prop> \(NonEmpty ps) -> length ps <= expandProductionCycle_measure ps

-}
expandProductionCycle_measure :: [DNSProduction e DNSMetaName t] -> Natural
expandProductionCycle_measure ps = n + (n * d)
 where
 n = genericLength ps
 d = expandProductionDepth ps

{- | for a cycle of mutually-recursive productions, take the 'maximum'
of their 'dnsExpand's.

since we must expand each production to the
same depth, and since each production can be configured with its own
depth, we need some aggregate.


-}
expandProductionDepth :: [DNSProduction e DNSMetaName t] -> Natural
expandProductionDepth
 = maximum
 . fmap (maybe 0 id)            -- each Nothing comes from DNSBuiltin
 . fmap (\p -> p ^? dnsProductionExpand)
 where
 dnsProductionExpand = dnsProductionName._Just.dnsMetaInfo.dnsExpand


inlineGrammar :: DNSGrammar DNSMetaName t -> DNSGrammar DNSMetaName t
inlineGrammar = id


vocabulariseGrammar :: DNSGrammar DNSMetaName t -> DNSGrammar DNSMetaName t
vocabulariseGrammar = id


compactGrammar :: DNSGrammar DNSMetaName t -> DNSGrammar DNSMetaName t
compactGrammar = id


