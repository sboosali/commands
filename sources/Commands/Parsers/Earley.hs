{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, LambdaCase, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables,  DeriveFunctor, DeriveDataTypeable , ViewPatterns, ConstraintKinds, DataKinds, TemplateHaskell                                     #-}
module Commands.Parsers.Earley where
import Commands.Etc
import           Commands.Symbol.Types
import Commands.LHS

import Data.List.NonEmpty    (NonEmpty (..))
import qualified Text.Earley           as E
import           Control.Lens          (( ^.)) -- Prism', 
import Data.ListLike (ListLike) 
import qualified Data.ListLike as ListLike
import Control.Monad.Catch (MonadThrow (..))
-- import Control.Exception.Lens              (handler)

import           Control.Applicative
import Data.Typeable (Typeable)
import Control.Exception (Exception) -- ,SomeException (..))
import qualified Data.Char as Char
-- import qualified Control.Exception


{- |

the type parameters:

* @z@ must be universally-quantified, always, to be passed to 'E.parser', which has a @Rank2@ type.
* @l@ as 'Rule'.
* @i@ as 'Rule'. (this is the token @t@ in 'E.Prod')

-- TODO Using 'Prod' over 'Grammar', we simplify the implementation, by giving up (mutually-)recursive productions. for now, that's okay, as most speech recognition engines don't suppor them.

a @newtype@ for "unsaturated" application.

-}
newtype EarleyProduction z l i a = EarleyProduction { unEarleyProduction :: E.Prod z l i a }
 deriving (Functor,Applicative,Alternative,Monoid) -- Profunctor
-- type EarleyProduction z l i a = Grammar z l (Prod z l i a)
-- type EarleyProduction z l i a = Prod z l i a

data EarleyError e ts = EarleyError { earleyReport :: E.Report e ts } deriving (Show,Typeable)
-- | for 'MonadThrow'
instance (Exceptional e, Exceptional ts) => Exception (EarleyError e ts)

-- | 'Right' when there is at least one parse that has consumed the whole input.
toEarleyError
 :: (ListLike ts t, (Exceptional e, Exceptional ts))
 => ([a], E.Report e ts)
 -> Either (EarleyError e ts) (NonEmpty a)
toEarleyError = \case
 ([],   e)                                    -> Left  $ EarleyError e 
 (x:xs, E.Report _ _ (ListLike.null -> True)) -> Right $ x:|xs
 (_,    e)                                    -> Left  $ EarleyError e 

{- |

the core glue between `commands-core` and `earley`

-}
induceEarleyProduction :: (Eq i) => l -> RHS (EarleyProduction z l) r l i a -> EarleyProduction z l i a
induceEarleyProduction lhs = EarleyProduction . (E.<?> lhs) . runRHS fromSymbol
 where
 fromSymbol :: (Eq i) => Symbol (Rule (EarleyProduction z l) r l) i a -> E.Prod z l i a -- signature is necessary
 fromSymbol = \case
  Terminal i                                  -> E.symbol i
  NonTerminal (Rule _ _ (EarleyProduction p)) -> p
  -- we can ignore the l: if the parser was automatically induced, <?> has been already called, as above; if the parser was manually written by the user, it is included exactly.

-- NonEmpty.fromList is safe because Prod's some is non-empty by definition:
-- instance Alternative (Prod r e t) where ... some p = (:) <$> p <*> many p

-- | always succeeds, matching the token.
anyToken :: E.Prod z LHS i i
anyToken = E.satisfy (const True) E.<?> unsafeLHSFromName 'anyToken

-- | matches any token that is all uppercase letters.
--
-- TODO spacing, casing, punctuation; may all be weird when letters are recognized by Dragon NaturallySpeaking.
anyLetter :: E.Prod z LHS String String
anyLetter = (E.satisfy (all Char.isUpper)) E.<?> unsafeLHSFromName 'anyLetter


{- | always succeeds, never matches.

identity:

@
unitEarleyParser '*>' p
p '<*' unitEarleyParser
@

-}
unitEarleyParser :: EarleyProduction z l i ()
unitEarleyParser = EarleyProduction $ pure ()

-- | 
runEarleyProduction :: (ListLike ts t) => (forall z. EarleyProduction z l t a) -> ts -> ([a], E.Report l ts)
runEarleyProduction p ts = E.fullParses (E.parser (E.rule (unEarleyProduction p)) ts)

-- | for ambiguous grammars, returns all possible parses. may throw an 'EarleyError'.
runRuleParser
 :: (ListLike ts t, (Exceptional l, Exceptional ts))
 => (forall z. Rule (EarleyProduction z l) r l t a)
 -> ts
 -> Possibly (NonEmpty a)
runRuleParser rule ts = case toEarleyError $ runEarleyProduction (rule^.ruleParser) ts of
 Left  e  -> throwM e
 Right xs -> return xs

-- -- | (specialized for convenience in GHCi.)
-- handleEarleyParser
--  :: (Exceptional l, Show a)
--  => (forall z. Rule (EarleyProduction z l) r l String a) -> String -> IO ()
-- handleEarleyParser p s = handles parseHandlers $ do
--  x <- runRuleParser p (words s) -- TODO runEarleyProduction
--  print x

-- -- | ("Control.Exception.Handler", not "Control.Monad.Catch.Handler" or "Control.Monad.Error.Lens.Handler".)
-- parseHandlers :: [Control.Exception.Handler ()]
-- parseHandlers =
--  [ handler _EarleyError $ \e -> do
--     print e
--     putStrLn ""
--  ]

-- -- | see <https://hackage.haskell.org/package/lens-4.7/docs/Control-Exception-Lens.html#g:6 Control.Exception.Lens>
-- _EarleyError :: (Exceptional e, Exceptional ts) => Prism' SomeException (EarleyError e ts)
-- _EarleyError = prismException
