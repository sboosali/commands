{-# LANGUAGE RankNTypes, LambdaCase #-}

{-| 

-}
module Commands.Parsers.Earley where 

import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Text.Earley                     as E
import qualified Text.Earley.Grammar             as E
import qualified Text.Earley.Internal            as E
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text) 

import           Control.Monad.ST
import           Data.STRef
import           Data.Char
import Control.Applicative
import Control.Arrow ((>>>))
import           Data.Function                   ((&) )


type EarleyEither e t = Either (E.Report e [t])

data EarleyParser s r e t a = EarleyParser
 { pProd :: (E.ProdR s r e t a)
 , pBest :: NonEmpty a -> a 
 -- , pRank :: a -> Int TODO law: pBest = argmax pRank , but can be optimized e.g. parallelized , with default record 
 }


-- ================================================================ --

{-| 

warning: uses "Text.Earley.Internal" 

-}
buildEarleyResult
 :: E.ProdR s a n t a
 -> [t]
 -> ST s (E.Result s n [t] a)
buildEarleyResult p ts = do
  s <- E.initialState p
  E.parse [s] (E.emptyParseEnv ts)

{-| 

warning: uses "Text.Earley.Internal" 

-}
buildEarleyNonTerminal
 :: n
 -> ST s (  E.ProdR s r n t a
         -> E.ProdR s r n t a
         )
buildEarleyNonTerminal n = do
 conts <- newSTRef =<< newSTRef []
 null  <- newSTRef Nothing
 return$ (\p -> E.NonTerminal (E.Rule p null conts) (E.Pure id) E.<?> n)

{-| wraps 'E.fullParses' 

-}
fullParsesE
 :: (forall r. E.Grammar r e (E.Prod r e t a)) 
 -> [t] 
 -> EarleyEither e t (NonEmpty a)
fullParsesE g = \ts ->
 toEarleyEither (E.fullParses (E.parser g ts))
{-# INLINEABLE fullParsesE #-}

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

bestParse :: (forall s r. EarleyParser s r e t a) -> [t] -> EarleyEither e t a
bestParse p ts = (p&pBest) <$> eachParse (p&pProd) ts

eachParse :: (forall s r. E.ProdR s r e t a) -> [t] -> EarleyEither e t (NonEmpty a)
eachParse p ts = toEarleyEither (E.fullParses (buildEarleyResult p ts))



-- ================================================================ --

{-| @unitEarleyParser = pure ()@ 

-}
unitEarleyParser :: E.Prod r n t () 
unitEarleyParser = pure ()

anyWord :: E.Prod r e t t 
anyWord = E.Terminal (const True) (pure id)

anyLetter :: E.Prod r String Text Text
anyLetter = (E.satisfy (T.all isUpper)) E.<?> "letter"

{-| comes from Dragon as:  

@
['spell', 'a', 'b', 'c'] TODO 
@ 

(after being munged from:) 

@
['spell', 'a\\spelling-letter\\A', 'b\\spelling-letter\\B', 'c\\spelling-letter\\C']
@ 



-}
anyLetters :: E.Prod r String Text Text
anyLetters = T.concat <$> some (E.satisfy isSingleLetter) E.<?> "letters"
 where
 isSingleLetter = T.uncons >>> \case
  Nothing -> False 
  Just (c, _) -> isAlphaNum c 

