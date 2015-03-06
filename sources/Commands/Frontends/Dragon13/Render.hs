{-# LANGUAGE DataKinds, ExistentialQuantification #-}
module Commands.Frontends.Dragon13.Render where
-- import           Commands.Etc
import Commands.Command.Types
import Commands.Frontends.Dragon13.Types
import Commands.Grammar
import Commands.Grammar.Types
import Control.Alternative.Free.Tree

import Control.Applicative
import Data.Foldable                     (foldMap)
import Data.List.NonEmpty                (NonEmpty (..), nonEmpty)
import Data.Maybe                        (catMaybes, fromMaybe, mapMaybe)
import Data.Monoid                       ((<>))

-- import           Data.Text.Lazy                    (Text)
-- import qualified Data.Text.Lazy                    as T


renderRule :: Rule x -> DNSGrammar String String
renderRule (Rule l r) = DNSGrammar
 (renderProduction $ Rule l r)
 (getChildren r)


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
renderSymbol :: Symbol x -> DNSRHS String String
renderSymbol = symbol
 (\(Word t) -> DNSTerminal $ DNSToken t)
 (\(Command {_grammar = DNSGrammar (DNSProduction lhs _) _}) -> DNSNonTerminal lhs)

-- |
emptyList :: DNSRHS String String
emptyList = DNSNonTerminal $ DNSList "emptyList"

-- |
--
-- TODO is this a traversal or something? over the functor
--
-- TODO remove unique s with equality instance
getChildren :: RHS x -> [DNSProduction False String String]
getChildren (Pure _)     = []
getChildren (Many rs)    = getChildren `foldMap` rs
getChildren (fs `App` x) = getChildren fs <> symbol (const [])
 (\Command {_grammar = DNSGrammar p ps} -> downcastDNSProduction p : ps) x
getChildren (fs :<*> xs) = getChildren fs <> getChildren xs
