{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, LambdaCase, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables                                       #-}
module Commands.Parsers.Earley where
import           Commands.Symbol.Types

import qualified Data.List.NonEmpty    as NonEmpty
import qualified Text.Earley           as E

import           Control.Applicative


{- |

-- TODO Using 'Prod' over 'Grammar', we simplify the implementation, by giving up (mutually-)recursive productions. for now, that's okay, as most speech recognition engines don't suppor them.

a newtype for partial application.

-}
newtype EarleyProduction z l i a = EarleyProduction { unEarleyProduction :: E.Prod z l i a }
 deriving (Functor,Applicative,Alternative,Monoid) -- Profunctor
-- type EarleyProduction z l i a = Grammar z l (Prod z l i a)
-- type EarleyProduction z l i a = Prod z l i a

{- |

-}
induceEarleyProduction :: (Eq i) => l -> RHS (EarleyProduction z l) r l i a -> EarleyProduction z l i a
induceEarleyProduction lhs = EarleyProduction . (E.<?> lhs) . runRHS fromSymbol
 where
 fromSymbol :: (Eq i) => Symbol (Rule (EarleyProduction z l) r l) i a -> E.Prod z l i a -- signature is necessary
 fromSymbol = \case
  Terminal i                                  -> E.symbol i
  NonTerminal (Rule _ _ (EarleyProduction p)) -> p
  -- we can ignore the l: if the parser was automatically induced, <?> has been already called, as above; if the parser was manually written by the user, it is included exactly.
  Opt         x                            -> optional                   (fromSymbol x)
  Many        x                            -> many                       (fromSymbol x)
  Some        x                            -> NonEmpty.fromList <$> some (fromSymbol x)
-- NonEmpty.fromListh is safe because Prod's some is non-empty by definition:
-- instance Alternative (Prod r e t) where ... some p = (:) <$> p <*> many p

runEarleyProduction :: forall l a. (forall z. EarleyProduction z l String a) -> String -> ([a], E.Report l [String])
runEarleyProduction p s = E.fullParses (E.parser (E.rule (unEarleyProduction p)) (words s))

-- runParser :: forall l a. (forall z. Rule z l String a) -> String -> ([a], E.Report l [String])
-- runParser (Rule _ _ p) = runEarleyProduction p

-- TODO understand:
-- when (1) the type is (inferred to be) rank1 (i.e. forall l a z. EarleyProduction z l String a), not rank2 (i.e. forall l a. (forall z. EarleyProduction z l String a))
-- or (2) matching (i.e. EarleyProduction p), not extracting (i.e. unEarleyProduction p),
-- we get a "type variable escaping scope" error:
--
--     Couldn't match type ‘z0’ with ‘r’
--       because type variable ‘r’ would escape its scope
--     This (rigid, skolem) type variable is bound by
--       a type expected by the context: E.Grammar r l (E.Prod r l String a)
--
-- "So, an escaped type variable occurs when you have a type bound by a quantifier that GHC infers should be unified with an undetermined type outside the scope of that quantifier."

