{-# LANGUAGE RankNTypes, LambdaCase, EmptyCase #-}
{-# LANGUAGE ScopedTypeVariables, LiberalTypeSynonyms, TypeFamilies, TemplateHaskell   #-}

{-|

-}
module Commands.Mixins.DNS13OSX9.Frontend where

import Commands.RHS
import Commands.Frontends.Dragon13
import           Commands.Extra hiding (gets, modify)
import Commands.Mixins.DNS13OSX9.Types
import Commands.Mixins.DNS13OSX9.Derived
import Commands.Mixins.DNS13OSX9.ObservedSharing

import qualified Data.Text.Lazy as T
import Data.Bifunctor(second, bimap)
import Data.Bitraversable(bitraverse)
import Control.Comonad.Cofree (Cofree(..))
-- import Control.Monad.Catch (MonadThrow (..))
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NonEmpty

import Data.Void
-- import           Data.Unique
import           Data.Function                   (on)
import Control.Monad.Trans.State
import Control.Exception (SomeException (..))
import qualified Data.List as List

-- unsafe
import           System.IO.Unsafe


-- ================================================================ --
--
-- {-|
--
-- -}
-- isFiniteDNSRHS :: DNSRHS t n -> IsFiniteGrammar t
-- isFiniteDNSRHS = \case
--  DNSMultiple{}        -> abortIsFiniteGrammar
--  DNSNonTerminal{}     -> ignoreIsFiniteGrammar
--  DNSTerminal t      -> (keepIsFiniteGrammar FiniteTerminal) (fromDNSToken t)
--  DNSSequence rs     -> (when2_ FiniteSequence)     <$> (traverse go rs)
--  DNSAlternatives rs -> (whenN_ FiniteAlternatives) <$> (traverse go rs)
--  DNSOptional r      -> (when1_ FiniteOptional)     <$> (go r)
--
--  where
--  go = isFiniteDNSRHS
--  when1_ f = fmap f
--  when2_ f = whenN_ (foldl1 f)   -- TODO partial function, safe because of whenN_
--  whenN_ f rs = (f . NonEmpty.toList) <$> nonEmpty (catMaybes (NonEmpty.toList rs))
--  fromDNSToken = \case
--   DNSToken t -> t
--   DNSPronounced t _ -> t     -- TODO pass around a IsFiniteGrammarConfig, chooses between written (e.g. to preload the cached parser) or spoken (e.g. homophone detection, if possible in any utterance)



-- ================================================================ --

renameRHSToDNS
 :: IO (      RHS (DNSEarleyName n) t (DNSEarleyFunc (DNSEarleyName n) t) a
       -> IO (RHS (DNSUniqueName n) t (DNSEarleyFunc (DNSUniqueName n) t) a))
renameRHSToDNS = renameDNSEarleyRHSIO $ \_ (ConstName (i, n)) -> do
 -- k <- hashUnique <$> newUnique
 let k = 0                      -- TODO assumes user given names are unique
 return$ DNSUniqueName i n k

{-|

-}
induceDNS
 :: (Eq t)
 => RHS (DNSUniqueName String) t (DNSEarleyFunc (DNSUniqueName String) t) a
 -> Cofree (DNSRHS t) (DNSInfo, String)
induceDNS = induceDNS' >>> \case
 SomeDNSNonTerminal (DNSRule ((i,n) :< r)) -> (i,n)         :< r
 r                                         -> defaultDNSLHS :< r -- use a dummy left hand side when needed

defaultDNSLHS :: (DNSInfo,String) -- TODO IO (DNSInfo,String) ? to be extra safe.
defaultDNSLHS = (defaultDNSInfo, showName 'defaultDNSLHS)

{-| the core glue between an 'RHS' and a 'DNSGrammar'

-}
induceDNS'
 -- :: RHS (DNSUniqueName n) t (DNSEarleyFunc (DNSUniqueName n) t) a
 :: (Eq t)
 => RHS (DNSUniqueName String) t (DNSEarleyFunc (DNSUniqueName String) t) a
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
  (getTerminalsDNSEarley rhs) -- TODO doesn't terminate on cyclic data
  rhs

unVoidDNSRHS :: DNSRHS t Void -> DNSRHS t n
unVoidDNSRHS = second (\case)

-- 1. collect Cofree's by name
-- 2. for each, project Cofree's to name
reifyDNSRHS :: forall i n t. (Eq n) => Cofree (DNSRHS t) (i,n) -> NonEmpty (DNSProduction i t n) --TODO All this shit
--  Map n (DNSProduction i t n) is more efficient but unordered
reifyDNSRHS = NonEmpty.fromList            --   TODO prove safety
 . fmap (snd >>> toIndirectDNSRHS >>> toDNSProduction)
 . (flip execState) []
 . go -- runMaybeT
 where

 toIndirectDNSRHS :: Cofree (DNSRHS t) (i,n) -> (i, n, DNSRHS t n)
 toIndirectDNSRHS ((i,n) :< r) = (i,n, bimap id (\((_,n') :< _) -> n') r)

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

serializeDNSGrammar' :: DnsOptimizationSettings -> DNSGrammar DNSInfo Text Text -> Either [SomeException] SerializedGrammar
serializeDNSGrammar' settings uG = do
 let oG = optimizeDNSInfoGrammar settings uG
 eG <- escapeDNSGrammar oG
 let sG = serializeGrammar eG
 return$ sG

serializeRHSAsDNSGrammar
 :: DnsOptimizationSettings
 -> RHS
                                    (DNSUniqueName String)
                                    Text
                                    (DNSEarleyFunc (DNSUniqueName String) Text)
                                    a
 -> Either [SomeException] SerializedGrammar
serializeRHSAsDNSGrammar settings
 = induceDNS >>> reifyDNSRHS >>> defaultDNSGrammar >>> second T.pack >>> serializeDNSGrammar' settings

formatRHS
 :: DnsOptimizationSettings
 -> RHS (DNSEarleyName String) Text (DNSEarleyFunc (DNSEarleyName String) Text) a
 -> IO (Either [SomeException] SerializedGrammar)
formatRHS settings rawRhs = do
 renamer <- renameRHSToDNS
 reifiedRhs <- renamer rawRhs
 let escapedGrammar = serializeRHSAsDNSGrammar settings reifiedRhs
 return$ escapedGrammar

showRHS :: DNSEarleyRHS a -> IO String
showRHS rawRhs = do
 escapedGrammar <- formatRHS defaultDnsOptimizationSettings rawRhs
 return$ either show displaySerializedGrammar escapedGrammar

{- | derive a grammar from a DNSEarleyRHS, by observing sharing.

("d" for DNS, "e" for Earley).

throws 'DNSGrammarException'

-}
de'deriveGrammarObservedSharing :: DnsOptimizationSettings -> DNSEarleyRHS a -> IO SerializedGrammar
de'deriveGrammarObservedSharing settings rhs = formatRHS settings rhs >>= either (throwM . DNSGrammarException) return

{-|

NOTE unsafe: calls 'unsafeSTToIO'.

-}
unsafeDNSGrammar :: DnsOptimizationSettings -> DNSEarleyRHS a -> SerializedGrammar
unsafeDNSGrammar settings r = unsafePerformIO$ de'deriveGrammarObservedSharing settings r  -- TODO lol
{-# NOINLINE unsafeDNSGrammar #-}
