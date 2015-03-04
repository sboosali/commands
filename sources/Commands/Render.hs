{-# LANGUAGE DataKinds, ExistentialQuantification, PackageImports #-}
module Commands.Render where
import           Commands.Etc
import           Commands.Frontends.Dragon13.Types
import           Commands.Grammar
import           Commands.Grammar.Types
import           Control.Alternative.Free.Tree

import           Control.Applicative
import           Data.Bifunctor                    (bimap)
import           Data.Functor.Constant
import           "transformers-compat" Data.Functor.Sum
import           Data.List.NonEmpty                (NonEmpty (..), nonEmpty)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (catMaybes, fromMaybe,
                                                    mapMaybe)
import           Data.Text.Lazy                    (Text)
import qualified Data.Text.Lazy                    as T


-- | renders a generic grammar to a Dragon NaturallySpeaking grammar.
render :: Rule x -> DNSGrammar Text Text
render = bimap T.pack T.pack . renderGrammar

-- |
--
-- uses the given 'Rule' as the 'dnsExport'.
renderGrammar :: Rule x -> DNSGrammar String String
renderGrammar (Rule l rs) = DNSGrammar export productions
 where
 export = renderProduction (Rule l rs)
 productions
  = fmap (\(l, Some rs) -> renderProduction (Rule l rs))
  . Map.toList
  . Map.delete l
  $ reifyRule (Rule l rs)

-- |
--
-- 'RHS' instantiate @Alternative@, and so may be @empty@. but 'DNSProduction' take @NonEmpty (DNSRHS n t)@. we must use Dragon's @{emptyList}@ for empty 'RHS's (later optimized away).
renderProduction :: Rule x -> DNSProduction e String String
renderProduction (Rule l r) = DNSProduction lhs rhs
 where
 lhs = renderLHS l
 rhs = renderRHS r

renderLHS :: LHS -> DNSLHS LHSRule String
renderLHS = DNSRule . showLHS

-- |
renderRHS :: RHS x -> NonEmpty (DNSRHS String String)
renderRHS r = case fromMaybe emptyList . renderRHS_ $ r of
 DNSAlternatives rs -> rs
 r -> r :| []

-- |
renderRHS_ :: RHS x -> Maybe (DNSRHS String String)
renderRHS_ (Pure _)     = Nothing
renderRHS_ (Many rs)    = DNSAlternatives <$> (nonEmpty . mapMaybe renderRHS_ $ rs)
renderRHS_ (fs `App` x) = DNSSequence <$> (nonEmpty . catMaybes $ [renderRHS_ fs, Just (renderSymbol x)])
renderRHS_ (fs :<*> xs) = DNSSequence <$> (nonEmpty . catMaybes $ [renderRHS_ fs, renderRHS_ xs])

-- |
renderSymbol :: Symb x -> DNSRHS String String
renderSymbol (InL (Constant (Word t))) = DNSTerminal $ DNSToken t
renderSymbol (InR (Rule l _)) = DNSNonTerminal $ renderLHS l

-- |
emptyList :: DNSRHS String String
emptyList = DNSNonTerminal $ DNSList "emptyList"

