{-# LANGUAGE DataKinds, GADTs, LambdaCase, TemplateHaskell #-}
module Commands.Frontends.Dragon13.Lens where
import Commands.Frontends.Dragon13.Types

import Control.Applicative
import Control.Lens
import Data.List.NonEmpty                (NonEmpty (..))


_DNSProduction :: Prism' (DNSProduction e n t) (DNSLHS LHSRule n, NonEmpty (DNSRHS n t))
_DNSProduction = prism' (uncurry DNSProduction) $ \case
 DNSProduction l rs -> Just (l, rs)
 _ -> Nothing

-- TODO.
-- not a Getter, because we want to set.
-- not a Prism, RHS (without LHS) is not enough to inject.
-- yes a Traversal, it can have zero-or-more targets and set them all (what are its laws?).
dnsProductionRHS :: Traversal' (DNSProduction e n t) (DNSRHS n t)
dnsProductionRHS = _DNSProduction._2.traversed

-- TODO.
-- not a Lens, because we can't set the existentially quantified LHS.
dnsProductionLHS :: Getter  (DNSProduction e n t) (SomeDNSLHS n)
dnsProductionLHS = to $ \case
 DNSProduction l _ -> SomeDNSLHS l
 DNSVocabulary l _ -> SomeDNSLHS l

dnsProductionName :: Traversal' (DNSProduction e n t) n
dnsProductionName f = \case
 DNSProduction l rs -> DNSProduction <$> dnsLHSName f l <*> pure rs
 DNSVocabulary l ts -> DNSVocabulary <$> dnsLHSName f l <*> pure ts

dnsSomeLHSName :: Traversal' (SomeDNSLHS n) n
dnsSomeLHSName f (SomeDNSLHS l) = SomeDNSLHS <$> dnsLHSName f l

-- not a Prism because it merges two cases
dnsLHSName :: Traversal' (DNSLHS l n) n
dnsLHSName f = \case
 DNSRule n    -> DNSRule <$> f n
 DNSList n    -> DNSList <$> f n
 l            -> pure l

makeLenses ''DNSGrammar
makePrisms ''DNSRHS
