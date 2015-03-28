{-# LANGUAGE DataKinds, ExistentialQuantification, NamedFieldPuns #-}
module Commands.Frontends.Dragon13.Render where
import Commands.Frontends.Dragon13.Lens
import Commands.Frontends.Dragon13.Types
import Commands.Grammar.Types
import Control.Alternative.Free.Tree

import Control.Applicative
import Control.Lens
import Data.List.NonEmpty                (nonEmpty)
import Data.Maybe                        (catMaybes, fromMaybe, mapMaybe)


-- |
--
-- 'RHS' instantiate @Alternative@, and so may be @empty@. but 'DNSProduction' take @NonEmpty (DNSRHS n t)@. we must use Dragon's @{emptyList}@ for empty 'RHS's (later optimized away).
renderRule :: Rule x -> DNSProduction DNSInfo DNSCommandName DNSCommandToken
renderRule (Rule l r) = DNSProduction defaultDNSInfo lhs rhs
 where
 lhs = renderLHS l
 rhs = renderRHS r

renderLHS :: LHS -> DNSLHS LHSRule DNSCommandName
renderLHS = DNSRule . defaultDNSExpandedName

-- |
renderRHS :: RHS x -> DNSRHS DNSCommandName DNSCommandToken
renderRHS = fromMaybe zeroDNSRHS . renderRHS_

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
 (\command -> DNSNonTerminal (SomeDNSLHS (command ^. comGrammar.dnsProductionLHS)))

