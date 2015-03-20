{-# LANGUAGE DataKinds, ExistentialQuantification, NamedFieldPuns #-}
module Commands.Frontends.Dragon13.Render where
import Commands.Command.Types            ()
import Commands.Etc
import Commands.Frontends.Dragon13.Types
import Commands.Grammar
import Commands.Grammar.Types
import Control.Alternative.Free.Tree

import Control.Applicative
import Data.Foldable                     (foldMap)
import Data.Function                     (on)
import Data.List                         (nub, nubBy)
import Data.List.NonEmpty                (NonEmpty (..), nonEmpty)
import Data.Maybe                        (catMaybes, fromMaybe, mapMaybe)


-- |
--
-- TODO slow because 'nub' has quadratic-time, but fast because it's really 'nubBy' on a part ('DNSLHS'), not the whole.
renderRule :: Rule x -> DNSGrammar DNSCommandName DNSCommandToken
renderRule rule = DNSGrammar
 (renderProduction rule)
 []
 (renderChildren rule)

-- |
--
-- 'RHS' instantiate @Alternative@, and so may be @empty@. but 'DNSProduction' take @NonEmpty (DNSRHS n t)@. we must use Dragon's @{emptyList}@ for empty 'RHS's (later optimized away).
renderProduction :: Rule x -> DNSProduction e DNSCommandName DNSCommandToken
renderProduction (Rule l r) = DNSProduction lhs rhs
 where
 lhs = renderLHS l
 rhs = renderRHS r

renderLHS :: LHS -> DNSLHS LHSRule DNSCommandName
renderLHS = DNSRule . showLHS -- defaultDNSMetaName

-- |
renderRHS :: RHS x -> NonEmpty (DNSRHS DNSCommandName DNSCommandToken)
renderRHS r = case fromMaybe emptyDNSRHS . renderRHS_ $ r of
 DNSAlternatives rs -> rs
 r -> r :| []

-- |
renderRHS_ :: RHS x -> Maybe (DNSRHS DNSCommandName DNSCommandToken)
renderRHS_ (Pure _)     = Nothing
renderRHS_ (Many rs)    = DNSAlternatives <$> (nonEmpty . mapMaybe renderRHS_ $ rs)
renderRHS_ (fs `App` x) = DNSSequence     <$> (nonEmpty . catMaybes $ [renderRHS_ fs, Just (renderSymbol x)])
renderRHS_ (fs :<*> xs) = DNSSequence     <$> (nonEmpty . catMaybes $ [renderRHS_ fs, renderRHS_ xs])

-- |
renderSymbol :: Symbol x -> DNSRHS DNSCommandName DNSCommandToken
renderSymbol = symbol
 (\(Word t) -> DNSTerminal $ DNSToken t)
 (\(Command {_grammar = DNSGrammar {_dnsExport = DNSProduction lhs _}}) -> DNSNonTerminal (SomeDNSLHS lhs))

-- emptyList :: DNSProduction False DNSCommandName DNSCommandToken
-- emptyList = DNSVocabulary (DNSNonTerminal $ DNSList "emptyList") []

-- |
--
-- excludes the current 'Command', to terminate.
renderChildren :: Rule x -> [DNSProduction False DNSCommandName DNSCommandToken]
renderChildren
 = nub
 . foldMap (\(Some (Command {_grammar = DNSGrammar p _ ps})) -> upcastDNSProduction p : ps)
 . nubBy ((==) `on` theLHS)
 . getChildren
 where
 theLHS = (\(Some (Command{_lhs})) -> _lhs)
