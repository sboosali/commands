{-# LANGUAGE DataKinds, NamedFieldPuns, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeFamilies, FlexibleContexts                 #-}
{- | combinators, i.e. higher-order grammatical rules.

-}
module Commands.Mixins.DNS13OSX9.Combinator where
import Commands.Etc
import Commands.Mixins.DNS13OSX9.Types
import           Commands.Sugar
import           Commands.Symbol.Types
import           Commands.Parsers.Earley
import           Commands.LHS
import   Commands.Frontends.Dragon13.Types

import           Control.Applicative
-- import           Control.Applicative.Permutation


-- | one or more of the grammar.
--
-- e.g. @(rule-+)@, using @-XPostfixOperators@
--
-- @(-+) = 'some' . 'toR'@
(-+) :: (ToRHS (EarleyProduction z LHS) DNSReifying LHS String h) => h -> RHS (EarleyProduction z LHS) DNSReifying LHS String [ToR h]
--(-+) :: (ToRHS p r l i h) => h -> RHS p r l i [ToR h]
(-+) = some . toR
-- (-+) :: (ToRHS p r l i h) => h -> RHS p r l i (NonEmpty (ToR h))
-- (-+) = Some id . toR

-- | zero or more of the grammar.
--
-- e.g. @(rule-*)@, using @-XPostfixOperators@
--
-- @(-*) = 'many' . 'toR'@
(-*) :: (ToRHS (EarleyProduction z LHS) DNSReifying LHS String h) => h -> RHS (EarleyProduction z LHS) DNSReifying LHS String [ToR h]
--(-*) :: (ToRHS p r l i h) => h -> RHS p r l i [ToR h]
(-*) = many . toR

-- | zero or one of the grammar.
--
-- e.g. @(rule-?)@, using @-XPostfixOperators@
--
-- @(-?) = 'optionalA' . 'toR'@
(-?) :: (ToRHS (EarleyProduction z LHS) DNSReifying LHS String h) => h -> RHS (EarleyProduction z LHS) DNSReifying LHS String (Maybe (ToR h))
--(-?) :: (ToRHS p r l i h) => h -> RHS p r l i (Maybe (ToR h))
(-?) = optionalA . toR

-- | zero or one of the grammar.
--
-- you can enrich your grammar with optionality, without polluting your datatype with Maybe's.
--
-- e.g. @(rule -?- default)@
--
(-?-) :: (ToRHS (EarleyProduction z LHS) DNSReifying LHS String h) => h -> (ToR h) -> RHS (EarleyProduction z LHS) DNSReifying LHS String (ToR h)
--(-?-) :: (ToRHS p r l i h) => h -> (ToR h) -> RHS p r l i (ToR h)
(-?-) x d = optionA d (toR x)

-- | either one grammar or the other.
--
-- e.g. @(rule1 -|- rule2 -|- rule3)@
--
-- @(-|-) x y = 'toR' x '<|>' 'toR' y@
(-|-)
 :: (ToRHS (EarleyProduction z LHS) DNSReifying LHS String h, ToRHS (EarleyProduction z LHS) DNSReifying LHS String j, a ~ ToR j, a ~ ToR h)
 => h -> j -> RHS (EarleyProduction z LHS) DNSReifying LHS String a
--(-|-)
 -- :: (ToRHS p r l i h, ToRHS p r l i j, a ~ ToR j, a ~ ToR h)
 -- => h -> j -> RHS p r l i a
(-|-) x y = toR x <|> toR y
infixr 3 -|-

-- | @= 'optionA' 'enumDefault' . 'toR'@
optionalEnum :: (Enum a) => R z a -> H z a
optionalEnum = optionA enumDefault . toR

