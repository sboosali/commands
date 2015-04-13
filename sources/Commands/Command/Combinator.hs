{-# LANGUAGE DataKinds, NamedFieldPuns, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell                   #-}
{- | 'Command' combinators. i.e. higher-order grammatical rules.

'manyC' and 'multipleC' share the same 'grammar' ('multipleDNSGrammar'), but different in 'parser' ('Parsec.many1' versus 'manyUntil' respectively).

'optionalC' and 'optionC' share the same 'grammar' ('optionalDNSGrammar'), but different in 'parser' ('Parsec.option' versus 'Parsec.optionMaybe' respectively).

every Grammar must have a unique name. asthese combinators are pure,
they must output a unique LHS from the LHSs of their input.
hence, 'LHSApp'.

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


-- | one or more of the grammar.
--
-- e.g. @(rule-+)@, using @-XPostfixOperators@
--
-- @(-+) = 'manyG'@
(-+) :: Grammar a -> Grammar [a]
(-+) = manyG

-- | zero or more of the grammar.
--
-- e.g. @(rule-*)@, using @-XPostfixOperators@
--
-- @(-*) = 'many0G'@
(-*) :: Grammar a -> Grammar [a]
(-*) = many0G

-- | zero or one of the grammar.
--
-- e.g. @(rule-?)@, using @-XPostfixOperators@
--
-- @(-?) = 'optionalG'@
(-?) :: Grammar a -> Grammar (Maybe a)
(-?) = optionalG

-- | either one grammar or the other.
--
-- e.g. @(rule-|)@, using @-XPostfixOperators@
--
-- @(-|) = 'eitherG'@
(-|) :: Grammar a -> Grammar b -> Grammar (Either a b)
(-|) = eitherG


-- ================================================================ --

-- | @= 'multipleG'@
multiple :: Grammar a -> Grammar [a]
multiple = multipleG

-- | @= 'manyG'@
many :: Grammar a -> Grammar [a]
many = manyG

-- | @= 'many0G'@
many0 :: Grammar a -> Grammar [a]
many0 = many0G

-- |
multipleG :: Grammar a -> Grammar [a]
multipleG grammar = Grammar
 (Rule l r)
 (oneOrMoreDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (\context -> (grammar ^. gramParser) context `manyUntil` context)
 where
 l = unsafeLHSFromName 'multipleG `appLHS` (grammar ^. gramLHS)
 r = multipleRHS grammar self
 self = multipleG grammar

-- |
multiple0G :: Grammar a -> Grammar [a]
multiple0G grammar = Grammar
 (Rule l r)
 (zeroOrMoreDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (\context -> (grammar ^. gramParser) context `many0Until` context)
 where
 l = unsafeLHSFromName 'multiple0G `appLHS` (grammar ^. gramLHS)
 r = multipleRHS grammar self
 self = multiple0G grammar

-- |
manyG :: Grammar a -> Grammar [a]
manyG grammar = Grammar
 (Rule l r)
 (oneOrMoreDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (\context -> Parsec.many1 $ (grammar ^. gramParser) context)
 where
 l = unsafeLHSFromName 'manyG `appLHS` (grammar ^. gramLHS)
 r = multipleRHS grammar self
 self = manyG grammar

-- |
many0G :: Grammar a -> Grammar [a]
many0G grammar = Grammar
 (Rule l r)
 (zeroOrMoreDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (\context -> Parsec.many $ (grammar ^. gramParser) context)
 where
 l = unsafeLHSFromName 'many0G `appLHS` (grammar ^. gramLHS)
 r = multipleRHS grammar self
 self = many0G grammar

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

-- | @= 'optionalG'@
optional :: Grammar a -> Grammar (Maybe a)
optional = optionalG

-- | @= 'optionG'@
option :: a -> Grammar a -> Grammar a
option = optionG

-- | @= 'optionG' 'enumDefault'@
optionalEnum :: (Enum a) => Grammar a -> Grammar a
optionalEnum = optionG enumDefault

-- |
optionG :: a -> Grammar a -> Grammar a
optionG theDefault grammar = Grammar
 (Rule l r)
 (optionalDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (\context -> Parsec.option theDefault $ (grammar ^. gramParser) context)
 where
 l = unsafeLHSFromName 'optionG `appLHS` (grammar ^. gramLHS)
 r = optionRHS theDefault grammar

-- |
optionalG :: Grammar a -> Grammar (Maybe a)
optionalG grammar = Grammar
 (Rule l r)
 (optionalDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (\context -> Parsec.optionMaybe $ (grammar ^. gramParser) context)
 where
 l = unsafeLHSFromName 'optionalG `appLHS` (grammar ^. gramLHS)
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
 = maybeAtomG
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
maybeAtomG :: Grammar a -> Perms RHS (Maybe a)
maybeAtomG grammar = (maybeAtom . liftGrammar) $ Grammar
 (Rule l r)
 (optionalDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (grammar ^. gramParser)
 where
 Rule l r = bimapRule (unsafeLHSFromName 'maybeAtomG `appLHS`) (const $ liftGrammar grammar) (grammar ^. gramRule)


-- | "yank"s (opposite of "hoist"?) a 'DNSProduction' down into a 'DNSRHS' by taking its 'DNSLHS',
-- and makes a new transformed 'DNSProduction'. and hints that it be 'dnsInline'd.
--
-- a helper function for defining higher-order productions.
yankDNSProduction :: (DNSRHS n t -> DNSRHS n t) -> n -> DNSProduction DNSInfo n t -> DNSProduction DNSInfo n t
yankDNSProduction f n (DNSProduction i l _) = DNSProduction
 (i & dnsInline .~ True)
 (DNSRule n)
 (f (DNSNonTerminal (SomeDNSLHS l)))


-- ================================================================ --

-- |
eitherG :: Grammar a -> Grammar b -> Grammar (Either a b)
eitherG a b  = set (gramGrammar .dnsProductionInfo .dnsInline) True $ genericGrammar
 (unsafeLHSFromName 'eitherG `LHSApp` [(a^.gramLHS),(b^.gramLHS)])
 (Left <$> liftGrammar a  <|>  Right <$> liftGrammar b)

-- TODO generalize to oneOfG, that takes a Record, and is implicitly
-- No, a Co-record! Google "CoRec" in the frames library.
-- (:&) won't look like (&) after the latter is renamed. It's a logical-or though, and (:&) looks like logical-and. We should probably just make another alias involving "|".
