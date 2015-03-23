{-# LANGUAGE DataKinds, NamedFieldPuns, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell                   #-}
{- | 'Command' combinators. i.e. higher-order grammatical rules.

'manyC' and 'multipleC' share the same 'grammar' ('multipleDNSGrammar'), but different in 'parser' ('Parsec.many1' versus 'manyUntil' respectively).

'optionalC' and 'optionC' share the same 'grammar' ('optionalDNSGrammar'), but different in 'parser' ('Parsec.option' versus 'Parsec.optionMaybe' respectively).


-}
module Commands.Command.Combinator where
import           Commands.Command
import           Commands.Etc
import           Commands.Frontends.Dragon13
-- import           Commands.Frontends.Dragon13.Lens
import           Commands.Frontends.Dragon13.Types
import           Commands.Grammar
import           Commands.Grammar.Types
import           Commands.Parsec

import           Control.Applicative.Permutation
import           Control.Lens
import qualified Text.Parsec                       as Parsec
import           Control.Applicative


-- ================================================================ --

-- | @= 'multipleC'@
multiple :: Command a -> Command [a]
multiple = multipleC

-- | @= 'manyC'@
many :: Command a -> Command [a]
many = manyC

-- | @= 'many0C'@
many0 :: Command a -> Command [a]
many0 = many0C

-- |
multipleC :: Command a -> Command [a]
multipleC command = Command
 (bimapRule (appLHS lhs) multipleRHS (command ^. comRule)) 
 (multipleDNSProduction (combinatorDNSCommandName lhs) (command ^. comGrammar))
 (\context -> (command ^. comParser) context `manyUntil` context)
 where
 Just lhs = lhsFromName 'multipleC

-- |
manyC :: Command a -> Command [a]
manyC command = Command
 (bimapRule (appLHS lhs) multipleRHS (command ^. comRule))
 (multipleDNSProduction (combinatorDNSCommandName lhs) (command ^. comGrammar))
 (\context -> Parsec.many1 $ (command ^. comParser) context)
 where
 Just lhs = lhsFromName 'manyC

-- |
many0C :: Command a -> Command [a]
many0C command = Command
 (bimapRule (appLHS lhs) multipleRHS (command ^. comRule))
 (zeroOrMoreDNSProduction (combinatorDNSCommandName lhs) (command ^. comGrammar))
 (\context -> Parsec.many $ (command ^. comParser) context)
 where
 Just lhs = lhsFromName 'many0C

-- |
multipleRHS :: RHS a -> RHS [a]
multipleRHS r = pure [] <|> (:) <$> r <*> multipleRHS r

-- |
multipleDNSProduction :: DNSCommandName -> DNSProduction True DNSCommandName t -> DNSProduction True DNSCommandName t
multipleDNSProduction = pushDNSProduction DNSMultiple

-- | 'DNSMultiple' recognizes one or more, @('DNSOptional' . 'DNSMultiple') recognizes zero or more.
zeroOrMoreDNSProduction :: DNSCommandName -> DNSProduction True DNSCommandName t -> DNSProduction True DNSCommandName t
zeroOrMoreDNSProduction = pushDNSProduction (DNSOptional . DNSMultiple)


-- ================================================================ --

-- | @= 'optionalC'@
optional :: Command a -> Command (Maybe a)
optional = optionalC

-- | @= 'optionC'@
option :: a -> Command a -> Command a
option = optionC

-- | @= 'optionC' 'enumDefault'@
optionalEnum :: (Enum a) => Command a -> Command a
optionalEnum = optionC enumDefault

-- |
optionC :: a -> Command a -> Command a
optionC theDefault command = Command
 (bimapRule (appLHS lhs) (optionRHS theDefault) (command ^. comRule))
 (optionalDNSProduction (combinatorDNSCommandName lhs) (command ^. comGrammar))
 (\context -> Parsec.option theDefault $ (command ^. comParser) context)
 where
 Just lhs = lhsFromName 'optionC

-- |
optionalC :: Command a -> Command (Maybe a)
optionalC command = Command
 (bimapRule (appLHS lhs) optionalRHS (command ^. comRule))
 (optionalDNSProduction (combinatorDNSCommandName lhs) (command ^. comGrammar))
 (\context -> Parsec.optionMaybe $ (command ^. comParser) context)
 where
 Just lhs = lhsFromName 'optionalC

-- |
optionalRHS :: RHS a -> RHS (Maybe a)
optionalRHS r = pure Nothing <|> Just <$> r

-- |
optionRHS :: a -> RHS a -> RHS a
optionRHS x r = pure x <|> r

-- |
optionalDNSProduction :: DNSCommandName -> DNSProduction True DNSCommandName t -> DNSProduction True DNSCommandName t
optionalDNSProduction = pushDNSProduction DNSOptional


-- ================================================================ --

-- |
maybeAtomR :: RHS a -> Perms RHS (Maybe a)
maybeAtomR
 = maybeAtomC
 -- . set (grammar .dnsExport .dnsProductionName .dnsMetaInfo .dnsInline) True  -- TODO removes production but doesn't in-line RHS
 . unsafeFellRHS

-- |
unsafeFellRHS :: RHS a -> Command a
unsafeFellRHS rhs = genericCommand (unsafeLHSFromRHS rhs) rhs

-- | changes the '_comGrammar' (and thus the '_comLHS'), but not the
-- '_comParser'.
--
-- the "optionality" (but not the "permutability") must be reified
-- for the '_comGrammar' (by this function).
-- then, both '_comGrammar' and '_comParser'  are permuted by
-- 'runPerms' (not by this function) acting upon 'maybeAtom'.
--
--
maybeAtomC :: Command a -> Perms RHS (Maybe a)
maybeAtomC command = (maybeAtom . liftCommand) $ Command
 (bimapRule (appLHS lhs) id (command ^. comRule))
 (optionalDNSProduction (combinatorDNSCommandName lhs) (command ^. comGrammar))
 (command ^. comParser)
 where
 Just lhs  = lhsFromName 'maybeAtomC


combinatorDNSCommandName :: LHS -> DNSCommandName
combinatorDNSCommandName
 = set (dnsMetaInfo.dnsInline) True
 . defaultDNSMetaName

