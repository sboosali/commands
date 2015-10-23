{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE ExistentialQuantification              #-}
{-# LANGUAGE FlexibleContexts, KindSignatures, LambdaCase, TypeOperators              #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables           #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-| 

-}
module Commands.Mixins.DNS13OSX9.Types where 

import           Commands.RHS.Types
import Commands.Frontends.Dragon13
import qualified Commands.Backends.OSX as OSX

import qualified Text.Earley.Grammar             as E
import qualified Text.Earley.Internal            as E
import qualified Data.Text.Lazy as T
import           Data.List.NonEmpty              (NonEmpty (..))
import Control.Lens (Traversal',_1) 
import Control.Comonad.Cofree (Cofree) 

import           Data.Char
import Data.Void
import Control.Exception (Exception, SomeException)


-- ================================================================ --

-- | the Earley parse function takes a Rank2 type (forall r. E.Prod r ...) that it instantiates to (forall s. (E.Rule s a)); then runST takes a Rank2 type (forall s. ...s...). this package exposes the internals of Earley, but of course not of ST. this type synonym is for convenience.
type RULED f s a = f (E.Rule s a) a
-- needs LiberalTypeSynonyms when f is a type synonym, I think.

type EarleyProd r = E.Prod r String Text

type R z a = DNSEarleyRHS z a

type R_ a = forall z. DNSEarleyRHS z a

type C z a = DNSEarleyCommand z a

type C_ a = forall z. DNSEarleyCommand z a


-- ================================================================ --

type DNSEarleyRHS z = RHS
 (DNSEarleyName String)
 Text
 (DNSEarleyFunc z (DNSEarleyName String) Text)

type DNSEarleyName n = ConstName (DNSInfo, n)

data DNSEarleyFunc z n t a
 = LeafRHS (E.Prod z String t a) (DNSRHS t Void)
 | TreeRHS (RHS n t (DNSEarleyFunc z n t) a) (RHS n t (DNSEarleyFunc z n t) a)
-- couples parser (E.Prod) with format (DNSRHS) with (ConstName) :-(

deriving instance (Functor (n t (DNSEarleyFunc z n t))) => Functor (DNSEarleyFunc z n t) --TODO UndecidableInstances
-- Variables ‘n, t’ occur more often than in the instance head in the constraint

type DNSEarleyCommand z = Command
 (DNSEarleyName String)
 Text
 (DNSEarleyFunc z (DNSEarleyName String) Text)
 OSX.Application
 OSX.CWorkflow_


-- ================================================================ --

type EarleyParser_ a = forall r. RULED EarleyParser r a

data EarleyParser z a = EarleyParser
 { pProd :: E.Prod z String Text a
 , pBest :: NonEmpty a -> a 
 }
 
data EarleyName z n t (f :: * -> *) a = EarleyName
 { unEarleyName :: E.Prod z n t a -> E.Prod z n t a
 }
-- not a Functor


-- ================================================================ --

data DNSFixName t = DNSFixName (DNSProduction DNSInfo (DNSFixName t) t) --TODO newtype

-- | a directly-recursive right-hand side, with a left-hand side annotation; like a production.
type DNSRHSRec i t n = Cofree (DNSRHS t) (i, n)
-- DNSRHS t (Cofree (DNSRHS t) (i, n))

data DNSUniqueName n t (f :: * -> *) a = DNSUniqueName DNSInfo n Int

newtype DNSGrammarException = DNSGrammarException [SomeException]
 deriving (Show)
deriving instance Exception DNSGrammarException


-- ================================================================ --

liftLeaf :: forall a
                         (n :: * -> (* -> *) -> * -> *)
                         t
                         (z :: * -> * -> * -> *)
                         (n1 :: * -> (* -> *) -> * -> *)
                         t1.
                  E.Prod z String t1 a
                  -> DNSRHS t1 Void -> RHS n t (DNSEarleyFunc z n1 t1) a
liftLeaf p r = liftRHS (LeafRHS p r)

liftTree :: forall a
                         (n :: * -> (* -> *) -> * -> *)
                         t
                         (z :: * -> * -> * -> *)
                         (n1 :: * -> (* -> *) -> * -> *)
                         t1. 
                     RHS n1 t1 (DNSEarleyFunc z n1 t1) a
                  -> RHS n1 t1 (DNSEarleyFunc z n1 t1) a
                  -> RHS n  t  (DNSEarleyFunc z n1 t1) a
liftTree p r = liftRHS (TreeRHS p r)

anyWord :: E.Prod z String Text Text --TODO  t t
anyWord = E.Terminal (const True) (pure id)

anyLetter :: E.Prod z String Text Text
anyLetter = (E.satisfy (T.all isUpper)) E.<?> "letter"

_RHSInfo :: Traversal' (RHS (DNSEarleyName String) t f a) DNSInfo
_RHSInfo = _NonTerminal._1.unConstName._1

projectDNSEarleyFunc :: forall (t :: * -> * -> * -> *)
                                     (t1 :: * -> (* -> *) -> * -> *)
                                     t2
                                     t3.
                              DNSEarleyFunc t t1 t2 t3
                              -> Maybe
                                   (RHS t1 t2 (DNSEarleyFunc t t1 t2) t3,
                                    RHS t1 t2 (DNSEarleyFunc t t1 t2) t3)
projectDNSEarleyFunc = \case
 LeafRHS{} -> Nothing 
 TreeRHS pRHS gRHS -> Just (pRHS, gRHS) 

-- | reach into the func (mutually recursive with the rhs).  
getTerminalsDNSEarley
 :: forall z t n a. (Eq t)
 => (RHS n t (DNSEarleyFunc z n t) a)
 -> [t] 
getTerminalsDNSEarley = getTerminals' (const id) getTerminalsFromDNSEarleyFunc
 where                          -- TODO explicit signatures necessary. no let-generalization?
 getTerminalsFromDNSEarleyFunc :: (forall a.  DNSEarleyFunc z n t a -> [t])
 getTerminalsFromDNSEarleyFunc = (maybe [] getTerminalsFromBoth . projectDNSEarleyFunc)
 getTerminalsFromBoth :: (forall a. ((RHS n t (DNSEarleyFunc z n t) a), (RHS n t (DNSEarleyFunc z n t) a)) -> [t])
 getTerminalsFromBoth (pRHS,gRHS) = getTerminalsDNSEarley pRHS ++ getTerminalsDNSEarley gRHS

