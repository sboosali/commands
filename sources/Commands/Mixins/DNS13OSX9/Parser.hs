{-# LANGUAGE RankNTypes, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, LiberalTypeSynonyms, TypeFamilies             #-}

{-| 

-}
module Commands.Mixins.DNS13OSX9.Parser where 

import Commands.RHS.Types 
import Commands.Mixins.DNS13OSX9.Types 
import Commands.Mixins.DNS13OSX9.Derived 
import Commands.Mixins.DNS13OSX9.ObservedSharing 
import Commands.Frontends.Dragon13 (DNSInfo(..)) 
import Commands.Parsers.Earley

import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Text.Earley                     as E
import qualified Text.Earley.Internal            as E
import Data.Text.Lazy (Text) 

import           Control.Monad.ST
import           Data.Function                   ((&) )


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

{-| the core glue between an 'RHS' and an Earley 'E.Prod'uction. 

-}
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
runEarley r1 ts = do
 r2 <- renameRHSToEarley >>= ($ r1)
 buildEarleyResult (induceEarley r2) ts

e'ParseBest :: (forall r. RULED EarleyParser r a) -> [Text] -> EarleyEither String Text a
e'ParseBest p ts = (p&pBest) <$> e'ParseAll (p&pProd) ts

e'ParseAll :: (forall r. RULED EarleyProd r a) -> [Text] -> EarleyEither String Text (NonEmpty a)
e'ParseAll p ts = toEarleyEither (E.fullParses (buildEarleyResult p ts))

e'ParseList :: (forall r. RULED EarleyProd r a) -> [Text] -> [a]
e'ParseList p ts = fst (E.fullParses (buildEarleyResult p ts))

{- | derive a parser from a DNSEarleyRHS, by observing sharing. 

("d" for DNS, "e" for Earley).

TODO safe with 'unsafePerformIO'?
-}
-- de'deriveParserObservedSharing :: RULED DNSEarleyRHS s a -> ST s (E.Prod (E.Rule s a) String Text a)
de'deriveParserObservedSharing :: RULED DNSEarleyRHS s a -> ST s (RULED EarleyProd s a)
de'deriveParserObservedSharing r1 = do
 r2 <- renameRHSToEarley >>= ($ r1)
 return$ induceEarley r2

