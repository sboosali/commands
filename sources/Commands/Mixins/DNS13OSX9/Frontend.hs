{-# LANGUAGE RankNTypes, LambdaCase, EmptyCase #-}
{-# LANGUAGE ScopedTypeVariables, LiberalTypeSynonyms, TypeFamilies   #-}

{-| 

-}
module Commands.Mixins.DNS13OSX9.Frontend where  

import Commands.RHS.Types 
import Commands.Frontends.Dragon13
import           Commands.Extra
import Commands.Mixins.DNS13OSX9.Types 
import Commands.Mixins.DNS13OSX9.ObservedSharing 

import qualified Data.Text.Lazy as T
import Data.Bifunctor(second, bimap)
import Data.Bitraversable(bitraverse) 
import Control.Comonad.Cofree (Cofree(..)) 
import Control.Monad.Catch (MonadThrow (..))
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NonEmpty

import Data.Void
import Data.Monoid              ((<>))
import           Data.Unique
import           Data.Function                   (on)
import Control.Monad.Trans.State
import Control.Exception (SomeException (..))
import qualified Data.List as List


renameRHSToDNS :: IO (RHS (DNSEarleyName n) t (DNSEarleyFunc z (DNSEarleyName n) t) a -> IO (RHS (DNSUniqueName n) t (DNSEarleyFunc z (DNSUniqueName n) t) a))
renameRHSToDNS = renameDNSEarleyRHSIO $ \_ (ConstName (i, n)) -> do
 k <- hashUnique <$> newUnique
 return$ DNSUniqueName i n k

induceDNS
 :: (Eq t)
 => RHS (DNSUniqueName String) t (DNSEarleyFunc z (DNSUniqueName String) t) a
 -> Cofree (DNSRHS t) (DNSInfo, String)
induceDNS = induceDNS' >>> \case
 SomeDNSNonTerminal (DNSRule ((i,n) :< r)) -> (i,n)         :< r
 r                                         -> defaultDNSLHS :< r

defaultDNSLHS :: (DNSInfo,String)
defaultDNSLHS = (defaultDNSInfo,"defaultDNSLHS") -- TODO should be unique; quote it?

induceDNS' -- TODO doesn't terminate on cyclic data
 -- :: RHS (DNSUniqueName n) t (DNSEarleyFunc z (DNSUniqueName n) t) a
 :: (Eq t)
 => RHS (DNSUniqueName String) t (DNSEarleyFunc z (DNSUniqueName String) t) a
 -- -> (Cofree (DNSRHS t) (Maybe (i,n)))
 -> DNSRHS t (Cofree (DNSRHS t) (DNSInfo, String))
induceDNS' rhs = foldRHSWith
  (\(DNSUniqueName i n k) r -> SomeDNSNonTerminal$ DNSRule$ (i, n <> "_" <> (show k)) :< r)
  (DNSTerminal . DNSToken)
  (\case
   LeafRHS _ g -> unVoidDNSRHS g
   TreeRHS _ gRHS -> induceDNS' gRHS) -- auxiliary recursion, not a cata
  (UnitDNSRHS)
  (\r1 r2 -> DNSSequence (r1 :| [r2]))
  (maybe ZeroDNSRHS DNSAlternatives . NonEmpty.nonEmpty)
  (DNSOptional)
  (DNSOptional . DNSMultiple)
  (DNSMultiple)
  (getTerminalsDNSEarley rhs)
  rhs 

unVoidDNSRHS :: DNSRHS t Void -> DNSRHS t n
unVoidDNSRHS = second (\case)

-- 1. collect Cofree's by name
-- 2. for each, project Cofree's to name
reifyDNSRHS :: forall i n t. (Eq n) => Cofree (DNSRHS t) (i,n) -> NonEmpty (DNSProduction i t n)
--  Map n (DNSProduction i t n) is more efficient but unordered
reifyDNSRHS = NonEmpty.fromList            --   TODO prove safety
 . fmap (snd >>> toIndirectDNSRHS >>> toDNSProduction)
 . (flip execState) []
 . go -- runMaybeT
 where

 toIndirectDNSRHS :: Cofree (DNSRHS t) (i,n) -> (i, n, DNSRHS t n)
 toIndirectDNSRHS ((i,n) :< r) = (i,n, bimap id (\((_,n) :< _) -> n) r)

 toDNSProduction :: (i, n, DNSRHS t n) -> DNSProduction i t n
 toDNSProduction (i,n,r) = DNSProduction i (DNSRule n) r

 list'insertBy :: (a -> a -> Bool) -> a -> [a] -> [a]
 list'insertBy eq x xs = case List.find (eq x) xs of
  Nothing -> snoc xs x          -- could reverse later; or like fuse it with a fold; or difference list; performance here doesn't matter
  Just{}  ->    xs

 go :: Cofree (DNSRHS t) (i,n) -> (State [(n, Cofree (DNSRHS t) (i,n))]) ()
 go c@((_,n) :< r) = do
  gets (List.lookup n) >>= \case
   Nothing -> do
    _ <- modify$ list'insertBy ((==) `on` fst) (n, c)
    _ <- bitraverse return go r
    return()
   Just {} -> return()

serializeDNSGrammar' :: DNSGrammar DNSInfo Text Text -> Either [SomeException] SerializedGrammar
serializeDNSGrammar' uG = do
 let oG = optimizeDNSInfoGrammar uG                    -- optimizeDNSInfoGrammar
 eG <- escapeDNSGrammar oG
 let sG = serializeGrammar eG
 return$ sG

formatRHS :: RHS (DNSEarleyName String) Text (DNSEarleyFunc z (DNSEarleyName String) Text) a -> IO (Either [SomeException] SerializedGrammar)
formatRHS r = do
 renamer <- renameRHSToDNS
 let serializeRHS = (induceDNS >>> reifyDNSRHS >>> defaultDNSGrammar >>> second T.pack >>> serializeDNSGrammar')
 eG <- renamer r >>= (serializeRHS >>> return)
 return$ eG

showRHS :: RHS (DNSEarleyName String) Text (DNSEarleyFunc z (DNSEarleyName String) Text) a -> IO Text
showRHS r = do
 eG <- formatRHS r
 return$ either (T.pack . show) displaySerializedGrammar eG

{- | derive a grammar from a DNSEarleyRHS, by observing sharing. 

("d" for DNS, "e" for Earley).

-}
de'deriveGrammarObservedSharing :: DNSEarleyRHS z a -> IO SerializedGrammar
de'deriveGrammarObservedSharing rhs = do --TODO may throw exception 
 g <- formatRHS rhs >>= \case
  Right g  -> return g
  Left  es -> throwM$ DNSGrammarException es
 return g

