{-# LANGUAGE RankNTypes, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, LiberalTypeSynonyms, TypeFamilies             #-}

{-| 

-}
module Commands.Mixins.DNS13OSX9.Parser where 

import Commands.RHS.Types 
import           Commands.Extra
import Commands.Mixins.DNS13OSX9.Types 
import Commands.Mixins.DNS13OSX9.ObservedSharing 
import Commands.Frontends.Dragon13 -- TODO 
import Commands.Parsers.Earley

import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Text.Earley                     as E
import qualified Text.Earley.Internal            as E
import qualified Data.Text.Lazy as T
-- import Data.Text.Lazy (Text) 
import Data.Bifunctor(second) 
import Control.Monad.Catch (MonadThrow (..))

import           Control.Monad.ST
import           Data.Function                   ((&) )


-- renameRHSToEarley
--  :: forall s r n t f a. ()
--  => ST s (        RHS (ConstName                 n) t f a
--          -> ST s (RHS (EarleyName (E.Rule s r) n) t f a)
--          )
-- renameRHSToEarley = renameRHSST $ \_ (ConstName n) _ -> do
-- renameRHSToEarley :: ST s (RHS (ConstName (_, n)) t (DNSEarleyFunc z (ConstName (_, n)) t) a -> ST s (RHS (EarleyName (E.Rule s r) n) t (DNSEarleyFunc z (EarleyName (E.Rule s r) n) t) a))
renameRHSToEarley
 :: ST s (        DNSEarleyRHS (E.Rule s r) a
         -> ST s (RHS (EarleyName (E.Rule s r) String)
                      Text
                      (DNSEarleyFunc (E.Rule s r)
                      (EarleyName (E.Rule s r) String) Text)
                      a)
         )
renameRHSToEarley = renameDNSEarleyRHSST $
 \_ (ConstName (_, n)) -> EarleyName <$> buildEarleyNonTerminal n 

induceEarley
 :: forall s r n t a z. ((z ~ E.Rule s r), (n ~ String))  -- type equality only for documentation
 => (Eq t)
 => RHS (EarleyName z n)
        t
        (DNSEarleyFunc z (EarleyName z n) t)
        a
 -> E.Prod z n t a
induceEarley rhs = runRHSWith
 (\n r -> (unEarleyName n) (induceEarley r))  -- accessor (not pattern match) for polymorphic z (Rank2 elsewhere)
 E.symbol
 (\case
  LeafRHS p    _ -> p
  TreeRHS pRHS _ -> induceEarley pRHS)
 (getTerminalsDNSEarley rhs) 
 rhs 

runEarley
 :: (forall r. RHS (ConstName (DNSInfo,String)) Text (DNSEarleyFunc (E.Rule s r) (ConstName (DNSInfo,String)) Text) a)
 -> [Text]
 -> ST s (E.Result s String [Text] a)
-- runEarley
--  :: (Eq t)
--  => (forall r. RHS (ConstName (_,String)) t (DNSEarleyFunc (E.Rule s r) (ConstName (_,String)) t) a)
--  -> [t]
--  -> ST s (E.Result s String [t] a)
runEarley r1 ts = do
 r2 <- renameRHSToEarley >>= ($ r1)
 buildEarleyResult (induceEarley r2) ts

parseRaw
 :: (forall r. RHS (ConstName (DNSInfo,String)) Text (DNSEarleyFunc r (ConstName (DNSInfo,String)) Text) a)
 -> [Text]
 -> ([a], E.Report String [Text])
parseRaw r ts = E.fullParses$ runEarley r ts

e'ParseBest :: (forall r. RULED EarleyParser r a) -> [Text] -> EarleyEither String Text a
e'ParseBest p ts = (p&pBest) <$> e'ParseAll (p&pProd) ts

e'ParseAll :: (forall r. RULED EarleyProd r a) -> [Text] -> EarleyEither String Text (NonEmpty a)
e'ParseAll p ts = toEarleyEither (E.fullParses (buildEarleyResult p ts))
 -- where
 -- report = E.fullParses result
 -- result = buildEarley (p&pProd) ts

e'ParseList :: (forall r. RULED EarleyProd r a) -> [Text] -> [a]
e'ParseList p ts = fst (E.fullParses (buildEarleyResult p ts))

-- | may 'throwM' a @('E.Report' String Text)@
parseThrow
 :: (forall z. DNSEarleyRHS z a)
 -> [Text]
 -> Possibly (NonEmpty a)
parseThrow r = parseEither r >>> \case
 Left  e  -> throwM e
 Right xs -> return xs

parseEither
 :: (forall r. RHS (ConstName (DNSInfo,String)) Text (DNSEarleyFunc r (ConstName (DNSInfo,String)) Text) a)
 -> [Text]
 -> EarleyEither String Text (NonEmpty a)
parseEither r
 = toEarleyEither
 . parseRaw r

parseText
 :: (forall r. DNSEarleyRHS r a) -- Couldn't match type ‘z1’ with ‘r’ because type variable ‘r’ would escape its scope
 -> [Text]
 -> EarleyEither String Text (NonEmpty a)
parseText r = parseEither r --  TODO

parseBest
 :: (NonEmpty a -> a)
 -> (forall r. DNSEarleyRHS r a)
 -> [Text]
 -> EarleyEither String Text a --TODO use Possibly?
parseBest best r = second best . parseText r

parseList :: (forall r. DNSEarleyRHS r a) -> [Text] -> [a]
parseList r ts = as
 where (as,_) = parseRaw r ts

parseString :: (forall r. DNSEarleyRHS r a) -> String -> [a]
parseString r = parseList r . (T.words . T.pack)

{- | derive a parser from a DNSEarleyRHS, by observing sharing. 

("d" for DNS, "e" for Earley).

TODO safe with 'unsafePerformIO'?
-}
-- de'deriveParserObservedSharing :: RULED DNSEarleyRHS s a -> ST s (E.Prod (E.Rule s a) String Text a)
de'deriveParserObservedSharing :: RULED DNSEarleyRHS s a -> ST s (RULED EarleyProd s a)
de'deriveParserObservedSharing r1 = do
 r2 <- renameRHSToEarley >>= ($ r1)
 return$ induceEarley r2

