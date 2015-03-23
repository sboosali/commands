{-# LANGUAGE DataKinds, ExistentialQuantification, NamedFieldPuns #-}
module Commands.Frontends.Dragon13.Render where
import Commands.Command.Types            ()
-- import Commands.Etc
import Commands.Frontends.Dragon13.Types
-- import Commands.Grammar
import Commands.Grammar.Types
import Control.Alternative.Free.Tree

import Control.Applicative
-- import Data.Foldable                     (foldMap)
-- import Data.Function                     (on)
-- import Data.List                         (nub, nubBy)
import Data.List.NonEmpty                (NonEmpty (..), nonEmpty)
import Data.Maybe                        (catMaybes, fromMaybe, mapMaybe)


renderRule :: Rule x -> DNSProduction True DNSCommandName DNSCommandToken
renderRule = renderProduction

-- |
--
-- 'RHS' instantiate @Alternative@, and so may be @empty@. but 'DNSProduction' take @NonEmpty (DNSRHS n t)@. we must use Dragon's @{emptyList}@ for empty 'RHS's (later optimized away).
renderProduction :: Rule x -> DNSProduction True DNSCommandName DNSCommandToken
renderProduction (Rule l r) = DNSProduction lhs rhs
 where
 lhs = renderLHS l
 rhs = renderRHS r

renderLHS :: LHS -> DNSLHS LHSRule DNSCommandName
renderLHS = DNSRule . defaultDNSMetaName

-- |
renderRHS :: RHS x -> NonEmpty (DNSRHS DNSCommandName DNSCommandToken)
renderRHS r = case fromMaybe zeroDNSRHS . renderRHS_ $ r of
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
 (\(Word t) -> DNSTerminal (DNSToken t))
 (\(Command {_comGrammar = DNSProduction lhs _}) -> DNSNonTerminal (SomeDNSLHS lhs))

