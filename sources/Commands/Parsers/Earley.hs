{-# LANGUAGE RankNTypes, LambdaCase #-}

{-| 

-}
module Commands.Parsers.Earley where 

import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Text.Earley                     as E
import qualified Text.Earley.Grammar             as E
import qualified Text.Earley.Internal            as E

import           Control.Monad.ST
import           Data.STRef


type EarleyEither e t = Either (E.Report e [t])

{-| 

(warning: uses "Text.Earley.Internal") 

-}
buildEarleyResult
 :: E.Prod (E.Rule s a) n t a
 -> [t]
 -> ST s (E.Result s n [t] a)
buildEarleyResult p ts = do
  s <- E.initialState p
  E.parse [s] (E.emptyParseEnv ts)

{-| 

(warning: uses "Text.Earley.Internal") 

-}
buildEarleyNonTerminal
 :: n
 -> ST s (  E.Prod (E.Rule s r) n t a
         -> E.Prod (E.Rule s r) n t a
         )
buildEarleyNonTerminal n = do
 conts <- newSTRef =<< newSTRef []
 null  <- newSTRef Nothing
 return$ (\p -> E.NonTerminal (E.Rule p null conts) (E.Pure id) E.<?> n)

{-| 

-}
parseEarley 
 :: (forall r. E.Grammar r e (E.Prod r e t a)) 
 -> [t] 
 -> EarleyEither e t (NonEmpty a)
parseEarley g = \ts ->
 toEarleyEither (E.fullParses (E.parser g ts))
{-# INLINEABLE parseEarley #-}

{-| @unitEarleyParser = pure ()@ 

-}
unitEarleyParser :: E.Prod r n t () 
unitEarleyParser = pure ()

{-| refine an 'E.Report', forcing the results.

'Right' when there is at least one parse that has consumed the whole input.

-}
toEarleyEither
 :: ([a], E.Report e [t])
 -> EarleyEither e t (NonEmpty a)
toEarleyEither = \case
 ([],   e)               -> Left  e
 (x:xs, E.Report _ _ []) -> Right (x:|xs)
 (_,    e)               -> Left  e

