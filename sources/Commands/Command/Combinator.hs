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
-- import           Control.Alternative.Free.Tree

-- import           Control.Applicative
import           Control.Applicative.Permutation
import qualified Text.Parsec                       as Parsec


-- | just an unqualified nickname
multiple :: Command a -> Command [a]
multiple = multipleC

-- | just an unqualified nickname
optional :: Command a -> Command (Maybe a)
optional = optionalC


multipleC :: Command a -> Command [a]
multipleC Command{_lhs,_grammar,_parser} = Command
 lhs
 (multipleDNSGrammar (showLHS lhs) _grammar)
 -- (\context -> _parser (Some parserUnit) `manyUntil` context) -- assumes _parser is context free
 (\context -> _parser context `manyUntil` context)
 where
 lhs = unsafeLHSFromName 'multipleC `LHSApp` [_lhs]

multipleDNSGrammar :: String -> DNSGrammar String t -> DNSGrammar String t
multipleDNSGrammar name (DNSGrammar production productions) = DNSGrammar
 (DNSProduction (DNSRule name) (hoistDNSRHS DNSMultiple production))
 (upcastDNSProduction production : productions)


-- |  TODO reify optionality for grammar, and keep this note in the documentation
maybeAtomR :: RHS a -> Perms RHS (Maybe a)
maybeAtomR = maybeAtomC . unsafeFellRHS

unsafeFellRHS :: RHS a -> Command a
unsafeFellRHS rhs = genericCommand (unsafeLHSFromRHS rhs) rhs

-- | changes the '_grammar' (and thus the '_lhs'), but not the '_parser'. the optionality (but not the "permutability") must be reified for the '_grammar', while both '_grammar' and '_parser' are then permuted by 'runPerms' acting upon 'maybeAtom'.
maybeAtomC :: Command a -> Perms RHS (Maybe a)
maybeAtomC Command{_lhs,_grammar,_parser} = (maybeAtom . liftCommand) $ Command lhs grammar parser
 where
 lhs     = unsafeLHSFromName 'maybeAtomC `LHSApp` [_lhs]
 grammar = (optionalDNSGrammar (showLHS lhs) _grammar)
 parser  = _parser
 -- = optAtom Nothing . liftCommand . optionalC


optionalEnum :: (Enum a) => Command a -> Command a
optionalEnum = optionC enumDefault

optionC :: a -> Command a -> Command a
optionC theDefault Command{_lhs,_grammar,_parser} = Command
 lhs
 (optionalDNSGrammar (showLHS lhs) _grammar)
 (\context -> Parsec.option theDefault $ _parser context)
 where
 lhs = unsafeLHSFromName 'optionC `LHSApp` [_lhs]

optionalC :: Command a -> Command (Maybe a)
optionalC Command{_lhs,_grammar,_parser} = Command lhs grammar parser
 where
 lhs     = unsafeLHSFromName 'optionalC `LHSApp` [_lhs]
 grammar = (optionalDNSGrammar (showLHS lhs) _grammar)
 parser  = (\context -> Parsec.optionMaybe $ _parser context)

optionalDNSGrammar :: String -> DNSGrammar String t -> DNSGrammar String t
optionalDNSGrammar name (DNSGrammar production productions) = DNSGrammar
 (DNSProduction (DNSRule name) (hoistDNSRHS DNSOptional production))
 (upcastDNSProduction production : productions)

