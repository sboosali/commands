{-# LANGUAGE DataKinds, NamedFieldPuns, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell                   #-}
module Commands.Command.Combinator where
import           Commands.Command
import           Commands.Etc
import           Commands.Frontends.Dragon13
import           Commands.Frontends.Dragon13.Types
import           Commands.Grammar
import           Commands.Grammar.Types
import           Commands.Parsec

import           Control.Lens
import           Control.Applicative.Permutation
import qualified Text.Parsec                       as Parsec


-- | @= 'multipleC'@
multiple :: Command a -> Command [a]
multiple = multipleC

-- |
multipleC :: Command a -> Command [a]
multipleC Command{_lhs,_grammar,_parser} = Command
 lhs
 (multipleDNSGrammar (combinatorDNSCommandName lhs) _grammar)
 (\context -> _parser context `manyUntil` context)
 -- (\context -> _parser (Some parserUnit) `manyUntil` context) -- forces _parser to the context free
 where
 lhs    = l `LHSApp` [_lhs]
 Just l = lhsFromName 'multipleC

-- |
multipleDNSGrammar :: DNSCommandName -> DNSGrammar DNSCommandName t -> DNSGrammar DNSCommandName t
multipleDNSGrammar name (DNSGrammar{_dnsExport,_dnsImports,_dnsProductions}) = DNSGrammar
 (DNSProduction (DNSRule name) (hoistDNSRHS DNSMultiple _dnsExport))
 _dnsImports
 (upcastDNSProduction _dnsExport : _dnsProductions)



-- | @= 'optionalC'@
optional :: Command a -> Command (Maybe a)
optional = optionalC

-- | @= 'optionC' 'enumDefault'@
optionalEnum :: (Enum a) => Command a -> Command a
optionalEnum = optionC enumDefault

-- |
optionC :: a -> Command a -> Command a
optionC theDefault Command{_lhs,_grammar,_parser} = Command lhs
 (optionalDNSGrammar (combinatorDNSCommandName lhs) _grammar)
 (\context -> Parsec.option theDefault $ _parser context)
 where
 lhs    = l `LHSApp` [_lhs]
 Just l = lhsFromName 'optionC

-- |
optionalC :: Command a -> Command (Maybe a)
optionalC Command{_lhs,_grammar,_parser} = Command lhs
 (optionalDNSGrammar (combinatorDNSCommandName lhs) _grammar)
 (\context -> Parsec.optionMaybe $ _parser context)
 where
 lhs    = l `LHSApp` [_lhs]
 Just l = lhsFromName 'optionalC

-- |
optionalDNSGrammar :: DNSCommandName -> DNSGrammar DNSCommandName t -> DNSGrammar DNSCommandName t
optionalDNSGrammar name (DNSGrammar{_dnsExport,_dnsImports,_dnsProductions}) = DNSGrammar
 (DNSProduction (DNSRule name) (hoistDNSRHS DNSOptional _dnsExport))
 _dnsImports
 (upcastDNSProduction _dnsExport : _dnsProductions)



-- |
maybeAtomR :: RHS a -> Perms RHS (Maybe a)
maybeAtomR = maybeAtomC . unsafeFellRHS

-- |
unsafeFellRHS :: RHS a -> Command a
unsafeFellRHS rhs = genericCommand (unsafeLHSFromRHS rhs) rhs

-- | changes the '_grammar' (and thus the '_lhs'), but not the
-- '_parser'.
--
-- the "optionality" (but not the "permutability") must be reified
-- for the '_grammar' (by this function).
-- then, both '_grammar' and '_parser'  are permuted by
-- 'runPerms' (not by this function) acting upon 'maybeAtom'.
--
--
maybeAtomC :: Command a -> Perms RHS (Maybe a)
maybeAtomC Command{_lhs,_grammar,_parser} = (maybeAtom . liftCommand) $ Command lhs grammar parser
 where
 lhs     = l `LHSApp` [_lhs]
 Just l  = lhsFromName 'maybeAtomC
 grammar = optionalDNSGrammar (combinatorDNSCommandName lhs) _grammar
 parser  = _parser


combinatorDNSCommandName :: LHS -> DNSCommandName
combinatorDNSCommandName
 = set (dnsMetaInfo.dnsInline) True
 . defaultDNSMetaName

