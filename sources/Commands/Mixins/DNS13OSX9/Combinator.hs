{-# LANGUAGE DataKinds, NamedFieldPuns, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell                   #-}
{- | 'Command' combinators. i.e. higher-order grammatical rules.

'manyC' and 'multipleC' share the same 'grammar' ('multipleDNSGrammar'), but different in 'parser' ('Parsec.many1' versus 'manyUntil' respectively).

'optionalC' and 'optionC' share the same 'grammar' ('optionalDNSGrammar'), but different in 'parser' ('Parsec.option' versus 'Parsec.optionMaybe' respectively).

every Grammar must have a unique name. asthese combinators are pure,
they must output a unique LHS from the LHSs of their input.
hence, 'LHSApp'.

-}
module Commands.Mixins.DNS13OSX9.Combinator where
import           Commands.Mixins.DNS13.Types
import           Commands.Mixins.DNS13OSX9.Types
import           Commands.Mixins.DNS13OSX9.Primitive
import           Commands.Etc
import           Commands.Frontends.Dragon13.Lens
import           Commands.Frontends.Dragon13.Types
import           Commands.Grammar
import           Commands.Grammar.Types
import           Commands.Parse.Types
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
(-+) :: G a -> G [a]
(-+) = manyG

-- | zero or more of the grammar.
--
-- e.g. @(rule-*)@, using @-XPostfixOperators@
--
-- @(-*) = 'many0G'@
(-*) :: G a -> G [a]
(-*) = many0G

-- | zero or one of the grammar.
--
-- e.g. @(rule-?)@, using @-XPostfixOperators@
--
-- @(-?) = 'optionalG'@
(-?) :: G a -> G (Maybe a)
(-?) = optionalG

-- | zero or one of the grammar.
--
-- you can enrich your grammar with optionality, without polluting your datatype with Maybe's.
--
-- e.g. @(rule -?- default)@
--
-- @(-?-) = flip 'optionG'@
(-?-) :: G a -> a -> G a
(-?-) = flip optionG

-- | either one grammar or the other.
--
-- e.g. @(rule-|)@, using @-XPostfixOperators@
--
-- @(-|) = 'eitherG'@
(-|) :: G a -> G b -> G (Either a b)
(-|) = eitherG


-- ================================================================ --

-- | @= 'multipleG'@
multiple :: G a -> G [a]
multiple = multipleG

-- | @= 'manyG'@
many :: G a -> G [a]
many = manyG

-- | @= 'many0G'@
many0 :: G a -> G [a]
many0 = many0G

-- |
multipleG :: G a -> G [a]
multipleG grammar = Grammar
 (Rule l r)
 (oneOrMoreDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (sensitiveParser (\context -> runSensitiveParser (grammar ^. gramParser) context `manyUntil` context))
 where
 l = unsafeLHSFromName 'multipleG `appLHS` (grammar ^. gramLHS)
 r = multipleRHS grammar self
 self = multipleG grammar

-- |
multiple0G :: G a -> G [a]
multiple0G grammar = Grammar
 (Rule l r)
 (zeroOrMoreDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (sensitiveParser (\context -> runSensitiveParser (grammar ^. gramParser) context `many0Until` context))
 where
 l = unsafeLHSFromName 'multiple0G `appLHS` (grammar ^. gramLHS)
 r = multipleRHS grammar self
 self = multiple0G grammar

-- |
manyG :: G a -> G [a]
manyG grammar = Grammar
 (Rule l r)
 (oneOrMoreDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (sensitiveParser (\context -> Parsec.many1 $ runSensitiveParser (grammar ^. gramParser) context))
 where
 l = unsafeLHSFromName 'manyG `appLHS` (grammar ^. gramLHS)
 r = multipleRHS grammar self
 self = manyG grammar

-- |
many0G :: G a -> G [a]
many0G grammar = Grammar
 (Rule l r)
 (zeroOrMoreDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (sensitiveParser (\context -> Parsec.many $ runSensitiveParser (grammar ^. gramParser) context))
 where
 l = unsafeLHSFromName 'many0G `appLHS` (grammar ^. gramLHS)
 r = multipleRHS grammar self
 self = many0G grammar

-- |
multipleRHS :: G a -> G [a] -> R [a]
multipleRHS c cs = pure [] <|> (:) <$> nont c <*> nont cs
-- TODO erase R from G totally, removing this functions.

-- |
oneOrMoreDNSProduction :: DNSReifyingName -> DNSProduction DNSInfo DNSReifyingName t -> DNSProduction DNSInfo DNSReifyingName t
oneOrMoreDNSProduction = yankDNSProduction DNSMultiple

-- | 'DNSMultiple' recognizes one or more, @('DNSOptional' . 'DNSMultiple')@ recognizes zero or more.
zeroOrMoreDNSProduction :: DNSReifyingName -> DNSProduction DNSInfo DNSReifyingName t -> DNSProduction DNSInfo DNSReifyingName t
zeroOrMoreDNSProduction = yankDNSProduction (DNSOptional . DNSMultiple)


-- ================================================================ --

-- | @= 'optionalG'@
optional :: G a -> G (Maybe a)
optional = optionalG

-- | @= 'optionG'@
option :: a -> G a -> G a
option = optionG

-- | @= 'optionG' 'enumDefault'@
optionalEnum :: (Enum a) => G a -> G a
optionalEnum = optionG enumDefault

-- |
optionG :: a -> G a -> G a
optionG theDefault grammar = Grammar
 (Rule l r)
 (optionalDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (sensitiveParser (\context -> Parsec.option theDefault $ runSensitiveParser (grammar ^. gramParser) context))
 where
 l = unsafeLHSFromName 'optionG `appLHS` (grammar ^. gramLHS)
 r = optionRHS theDefault grammar

-- |
optionalG :: G a -> G (Maybe a)
optionalG grammar = Grammar
 (Rule l r)
 (optionalDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (sensitiveParser (\context -> Parsec.optionMaybe $ runSensitiveParser (grammar ^. gramParser) context))
 where
 l = unsafeLHSFromName 'optionalG `appLHS` (grammar ^. gramLHS)
 r = optionalRHS grammar

-- -- |
-- optionalRHS :: R a -> R (Maybe a)
-- optionalRHS r = pure Nothing <|> Just <$> r  -- TODO doesn't seem to preserve lower-order rule

-- |
optionalRHS :: G a -> R (Maybe a)
optionalRHS c = pure Nothing <|> Just <$> nont c

-- -- |
-- optionRHS :: a -> R a -> R a
-- optionRHS x r = pure x <|> r

-- |
optionRHS :: a -> G a -> R a
optionRHS x c = pure x <|> nont c


-- |
optionalDNSProduction :: DNSReifyingName -> DNSProduction DNSInfo DNSReifyingName t -> DNSProduction DNSInfo DNSReifyingName t
optionalDNSProduction = yankDNSProduction DNSOptional


-- ================================================================ --

-- |
maybeAtomR :: R a -> Perms R (Maybe a)
maybeAtomR
 = maybeAtomG
 . set (gramGrammar .dnsProductionInfo .dnsInline) True
 . unsafeFellRHS

-- |
unsafeFellRHS :: R a -> G a
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
maybeAtomG :: G a -> Perms R (Maybe a)
maybeAtomG grammar = (maybeAtom . nont) $ Grammar
 (Rule l r)
 (optionalDNSProduction (defaultDNSExpandedName l) (grammar ^. gramGrammar))
 (grammar ^. gramParser)
 where
 Rule l r = bimapRule (unsafeLHSFromName 'maybeAtomG `appLHS`) (const $ nont grammar) (grammar ^. gramRule)



-- ================================================================ --

-- |
eitherG :: G a -> G b -> G (Either a b)
eitherG a b  = set (gramGrammar .dnsProductionInfo .dnsInline) True $ genericGrammar
 (unsafeLHSFromName 'eitherG `LHSApp` [(a^.gramLHS),(b^.gramLHS)])
 (Left <$> nont a  <|>  Right <$> nont b)


-- ================================================================ --

-- | "yank"s (opposite of "hoist"?) a 'DNSProduction' down into a 'DNSRHS' by taking its 'DNSLHS',
-- making a new transformed 'DNSProduction'. and hints that it be 'dnsInline'd.
--
-- a helper function for defining higher-order productions.
yankDNSProduction :: (DNSRHS n t -> DNSRHS n t) -> n -> DNSProduction DNSInfo n t -> DNSProduction DNSInfo n t
yankDNSProduction f n (DNSProduction i l _) = DNSProduction
 (i & dnsInline .~ True)
 (DNSRule n)
 (f (DNSNonTerminal (SomeDNSLHS l)))

