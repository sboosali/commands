{-# LANGUAGE DataKinds, NamedFieldPuns, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell                   #-}
{- | 'Command' combinators. i.e. higher-order grammatical rules.

'manyC' and 'multipleC' share the same 'grammar' ('multipleDNSGrammar'), but different in 'parser' ('Parsec.many1' versus 'manyUntil' respectively).

'optionalC' and 'optionC' share the same 'grammar' ('optionalDNSGrammar'), but different in 'parser' ('Parsec.option' versus 'Parsec.optionMaybe' respectively).


-}
module Commands.Command.Combinator where
import           Commands.Command
import           Commands.Etc
-- import           Commands.Frontends.Dragon13.Lens
import           Commands.Frontends.Dragon13.Types
import           Commands.Grammar
import           Commands.Grammar.Types
import           Commands.Parsec

import           Control.Applicative
import           Control.Applicative.Permutation
import           Control.Lens
import qualified Text.Parsec                       as Parsec


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
 (Rule l r)
 (oneOrMoreDNSProduction (defaultDNSExpandedName l) (command ^. comGrammar))
 (\context -> (command ^. comParser) context `manyUntil` context)
 where
 Rule l r = bimapRule (unsafeLHSFromName 'multipleC `appLHS`) multipleRHS (command ^. comRule)

-- |
manyC :: Command a -> Command [a]
manyC command = Command
 (Rule l r)
 (oneOrMoreDNSProduction (defaultDNSExpandedName l) (command ^. comGrammar))
 (\context -> Parsec.many1 $ (command ^. comParser) context)
 where
 Rule l r = bimapRule (unsafeLHSFromName 'manyC `appLHS`) multipleRHS (command ^. comRule)

-- |
many0C :: Command a -> Command [a]
many0C command = Command
 (Rule l r)
 (zeroOrMoreDNSProduction (defaultDNSExpandedName l) (command ^. comGrammar))
 (\context -> Parsec.many $ (command ^. comParser) context)
 where
 Rule l r = bimapRule (unsafeLHSFromName 'many0C `appLHS`) multipleRHS (command ^. comRule)

-- |
multipleRHS :: RHS a -> RHS [a]
multipleRHS r = pure [] <|> (:) <$> r <*> multipleRHS r

-- |
oneOrMoreDNSProduction :: DNSCommandName -> DNSProduction DNSInfo DNSCommandName t -> DNSProduction DNSInfo DNSCommandName t
oneOrMoreDNSProduction = yankDNSProduction DNSMultiple

-- | 'DNSMultiple' recognizes one or more, @('DNSOptional' . 'DNSMultiple') recognizes zero or more.
zeroOrMoreDNSProduction :: DNSCommandName -> DNSProduction DNSInfo DNSCommandName t -> DNSProduction DNSInfo DNSCommandName t
zeroOrMoreDNSProduction = yankDNSProduction (DNSOptional . DNSMultiple)


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
 (Rule l r)
 (optionalDNSProduction (defaultDNSExpandedName l) (command ^. comGrammar))
 (\context -> Parsec.option theDefault $ (command ^. comParser) context)
 where
 Rule l r = bimapRule (unsafeLHSFromName 'optionC `appLHS`) (optionRHS theDefault) (command ^. comRule)

-- |
optionalC :: Command a -> Command (Maybe a)
optionalC command = Command
 (Rule l r)
 (optionalDNSProduction (defaultDNSExpandedName l) (command ^. comGrammar))
 (\context -> Parsec.optionMaybe $ (command ^. comParser) context)
 where
 Rule l r = bimapRule (unsafeLHSFromName 'optionalC `appLHS`) optionalRHS (command ^. comRule)

-- |
optionalRHS :: RHS a -> RHS (Maybe a)
optionalRHS r = pure Nothing <|> Just <$> r  -- TODO doesn't seem to preserve lower-order rule

-- |
optionRHS :: a -> RHS a -> RHS a
optionRHS x r = pure x <|> r

-- |
optionalDNSProduction :: DNSCommandName -> DNSProduction DNSInfo DNSCommandName t -> DNSProduction DNSInfo DNSCommandName t
optionalDNSProduction = yankDNSProduction DNSOptional


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
 (Rule l r)
 (optionalDNSProduction (defaultDNSExpandedName l) (command ^. comGrammar))
 (command ^. comParser)
 where
 Rule l r = bimapRule (unsafeLHSFromName 'maybeAtomC `appLHS`) id (command ^. comRule)


-- | "yank"s (opposite of "hoist"?) a 'DNSProduction' down into a 'DNSRHS' by taking its 'DNSLHS',
-- and makes a new transformed 'DNSProduction'. and hints that it be 'dnsInline'd.
--
-- a helper function for defining higher-order productions.
yankDNSProduction :: (DNSRHS n t -> DNSRHS n t) -> n -> DNSProduction DNSInfo n t -> DNSProduction DNSInfo n t
yankDNSProduction f n (DNSProduction i l _) = DNSProduction
 (i & dnsInline .~ True)
 (DNSRule n)
 (f (DNSNonTerminal (SomeDNSLHS l)))

