{-# LANGUAGE DataKinds, NamedFieldPuns, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell                   #-}
{- | 'Command' combinators. i.e. higher-order grammatical rules.

'manyC' and 'multipleC' share the same 'grammar' ('multipleDNSGrammar'), but different in 'parser' ('Parsec.many1' versus 'manyUntil' respectively).

'optionalC' and 'optionC' share the same 'grammar' ('optionalDNSGrammar'), but different in 'parser' ('Parsec.option' versus 'Parsec.optionMaybe' respectively).


-}
module Commands.Command.Combinator where
import           Commands.Command
import           Commands.Etc
import           Commands.Frontends.Dragon13.Lens
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
multiple :: Grammar a -> Grammar [a]
multiple = multipleC

-- | @= 'manyC'@
many :: Grammar a -> Grammar [a]
many = manyC

-- | @= 'many0C'@
many0 :: Grammar a -> Grammar [a]
many0 = many0C

-- |
multipleC :: Grammar a -> Grammar [a]
multipleC grammar = Grammar
 (Rule l r)
 (oneOrMoreDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (\context -> (grammar ^. gramParser) context `manyUntil` context)
 where
 l = unsafeLHSFromName 'multipleC `appLHS` (grammar ^. gramLHS)
 r = multipleRHS grammar self
 self = multipleC grammar

-- |
manyC :: Grammar a -> Grammar [a]
manyC grammar = Grammar
 (Rule l r)
 (oneOrMoreDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (\context -> Parsec.many1 $ (grammar ^. gramParser) context)
 where
 l = unsafeLHSFromName 'manyC `appLHS` (grammar ^. gramLHS)
 r = multipleRHS grammar self
 self = manyC grammar

-- |
many0C :: Grammar a -> Grammar [a]
many0C grammar = Grammar
 (Rule l r)
 (zeroOrMoreDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (\context -> Parsec.many $ (grammar ^. gramParser) context)
 where
 l = unsafeLHSFromName 'many0C `appLHS` (grammar ^. gramLHS)
 r = multipleRHS grammar self
 self = many0C grammar

-- -- |
-- multipleRHS :: RHS a -> RHS [a]
-- multipleRHS r = pure [] <|> (:[]) <$> r  -- TODO horribly horrible, but the recursion causes non-termination.
-- -- multipleRHS r = pure [] <|> (:) <$> r <*> multipleRHS r
-- -- TODO you can't recur on RHS directly, you must recur through a Grammar, to terminate.
-- TODO horribly horrible, we shouldn't just ignore the RHS: we're not, the grammar contains it

-- |
multipleRHS :: Grammar a -> Grammar [a] -> RHS [a]
multipleRHS c cs = pure [] <|> (:) <$> liftGrammar c <*> liftGrammar cs

-- |
oneOrMoreDNSProduction :: DNSCommandName -> DNSProduction DNSInfo DNSCommandName t -> DNSProduction DNSInfo DNSCommandName t
oneOrMoreDNSProduction = yankDNSProduction DNSMultiple

-- | 'DNSMultiple' recognizes one or more, @('DNSOptional' . 'DNSMultiple') recognizes zero or more.
zeroOrMoreDNSProduction :: DNSCommandName -> DNSProduction DNSInfo DNSCommandName t -> DNSProduction DNSInfo DNSCommandName t
zeroOrMoreDNSProduction = yankDNSProduction (DNSOptional . DNSMultiple)


-- ================================================================ --

-- | @= 'optionalC'@
optional :: Grammar a -> Grammar (Maybe a)
optional = optionalC

-- | @= 'optionC'@
option :: a -> Grammar a -> Grammar a
option = optionC

-- | @= 'optionC' 'enumDefault'@
optionalEnum :: (Enum a) => Grammar a -> Grammar a
optionalEnum = optionC enumDefault

-- |
optionC :: a -> Grammar a -> Grammar a
optionC theDefault grammar = Grammar
 (Rule l r)
 (optionalDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (\context -> Parsec.option theDefault $ (grammar ^. gramParser) context)
 where
 l = unsafeLHSFromName 'optionC `appLHS` (grammar ^. gramLHS)
 r = optionRHS theDefault grammar

-- |
optionalC :: Grammar a -> Grammar (Maybe a)
optionalC grammar = Grammar
 (Rule l r)
 (optionalDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (\context -> Parsec.optionMaybe $ (grammar ^. gramParser) context)
 where
 l = unsafeLHSFromName 'optionalC `appLHS` (grammar ^. gramLHS)
 r = optionalRHS grammar

-- -- |
-- optionalRHS :: RHS a -> RHS (Maybe a)
-- optionalRHS r = pure Nothing <|> Just <$> r  -- TODO doesn't seem to preserve lower-order rule

-- |
optionalRHS :: Grammar a -> RHS (Maybe a)
optionalRHS c = pure Nothing <|> Just <$> liftGrammar c

-- -- |
-- optionRHS :: a -> RHS a -> RHS a
-- optionRHS x r = pure x <|> r

-- |
optionRHS :: a -> Grammar a -> RHS a
optionRHS x c = pure x <|> liftGrammar c


-- |
optionalDNSProduction :: DNSCommandName -> DNSProduction DNSInfo DNSCommandName t -> DNSProduction DNSInfo DNSCommandName t
optionalDNSProduction = yankDNSProduction DNSOptional


-- ================================================================ --

-- |
maybeAtomR :: RHS a -> Perms RHS (Maybe a)
maybeAtomR
 = maybeAtomC
 . set (gramGrammar .dnsProductionInfo .dnsInline) True
 . unsafeFellRHS

-- |
unsafeFellRHS :: RHS a -> Grammar a
unsafeFellRHS rhs = genericGrammar (unsafeLHSFromRHS rhs) rhs

-- | changes the '_gramGrammar' (and thus the '_gramLHS'), but not the
-- '_gramParser'.
--
-- the "optionality" (but not the "permutability") must be reified
-- for the '_gramGrammar' (by this function).
-- then, both '_gramGrammar' and '_gramParser'  are permuted by
-- 'runPerms' (not by this function) acting upon 'maybeAtom'.
--
--
maybeAtomC :: Grammar a -> Perms RHS (Maybe a)
maybeAtomC grammar = (maybeAtom . liftGrammar) $ Grammar
 (Rule l r)
 (optionalDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (grammar ^. gramParser)
 where
 Rule l r = bimapRule (unsafeLHSFromName 'maybeAtomC `appLHS`) (const $ liftGrammar grammar) (grammar ^. gramRule)


-- | "yank"s (opposite of "hoist"?) a 'DNSProduction' down into a 'DNSRHS' by taking its 'DNSLHS',
-- and makes a new transformed 'DNSProduction'. and hints that it be 'dnsInline'd.
--
-- a helper function for defining higher-order productions.
yankDNSProduction :: (DNSRHS n t -> DNSRHS n t) -> n -> DNSProduction DNSInfo n t -> DNSProduction DNSInfo n t
yankDNSProduction f n (DNSProduction i l _) = DNSProduction
 (i & dnsInline .~ True)
 (DNSRule n)
 (f (DNSNonTerminal (SomeDNSLHS l)))

