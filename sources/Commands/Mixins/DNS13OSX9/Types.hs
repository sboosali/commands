{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE ExistentialQuantification              #-}
{-# LANGUAGE FlexibleContexts, KindSignatures, LambdaCase, TypeOperators              #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables           #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}

{-| 

-}
module Commands.Mixins.DNS13OSX9.Types where 

-- import           Commands.Extra
import           Commands.RHS
import           Commands.Command.Types
import Commands.Frontends.Dragon13

import qualified Text.Earley.Grammar             as E
import qualified Text.Earley.Internal            as E
import Control.Lens (Traversal',_1,_2) 
import Control.Comonad.Cofree (Cofree) 

import Data.Void
import Control.Exception (Exception, SomeException)
import           Control.Monad.ST (ST) 


-- ================================================================ --

type R a = DNSEarleyRHS a

type C c a = DNSEarleyCommand c a


-- ================================================================ --

-- | 
type DNSEarleyRHS = RHS
 (DNSEarleyName String)
 Text
 (DNSEarleyFunc (DNSEarleyName String) Text)

-- | 
type DNSEarleyName n = ConstName (DNSInfo, n)

-- | 
data DNSEarleyFunc n t a -- TODO redistribute the sums 
 = LeafRHS (UnsafeEarleyProduction String t a) (DNSRHS t Void)
 | TreeRHS (RHS n t (DNSEarleyFunc n t) a) (RHS n t (DNSEarleyFunc n t) a)
-- couples parser (E.Prod) with format (DNSRHS) with (ConstName) :-(

deriving instance (Functor (n t (DNSEarleyFunc n t))) => Functor (DNSEarleyFunc n t) --TODO UndecidableInstances
-- Variables ‘n, t’ occur more often than in the instance head in the constraint

{-| existentially-quantified right hand side.  

-}
data SomeDNSEarleyRHS = forall x. -- TODO use RHS0 
 SomeDNSEarleyRHS { unSomeDNSEarleyRHS :: DNSEarleyRHS x } 

-- | 
type DNSEarleyCommand c = Command -- TODO remove 
 (DNSEarleyName String)
 Text
 (DNSEarleyFunc (DNSEarleyName String) Text)
 c

-- ================================================================ --

type DNSEarleyProd = UnsafeEarleyProduction String Text 

data UnsafeEarleyProduction e t a = forall s r. UnsafeEarleyProduction (E.Prod (E.Rule s r) e t a)
deriving instance Functor (UnsafeEarleyProduction e t)

data EarleyName s r n t (f :: * -> *) a = EarleyName
 { unEarleyName :: E.ProdR s r n t a -> ST s (E.ProdR s r n t a)
 }
-- not a Functor



-- ================================================================ --

-- | a directly-recursive right-hand side, with a left-hand side annotation; like a production.
type DNSRHSRec i t n = Cofree (DNSRHS t) (i, n)
-- DNSRHS t (Cofree (DNSRHS t) (i, n))

data DNSFixName t = DNSFixName (DNSProduction DNSInfo (DNSFixName t) t) --TODO newtype

data DNSUniqueName n t (f :: * -> *) a = DNSUniqueName DNSInfo n Int

newtype DNSGrammarException = DNSGrammarException [SomeException]
 deriving (Show)
deriving instance Exception DNSGrammarException


-- ================================================================ --

liftLeaf p r = liftRHS (LeafRHS p r)

liftTree p r = liftRHS (TreeRHS p r)

_DNSEarleyRHSInfo :: Traversal' (RHS (DNSEarleyName n) t f a) DNSInfo
_DNSEarleyRHSInfo = _RHSName.unConstName._1

_DNSEarleyRHSName :: Traversal' (RHS (DNSEarleyName String) t f a) String 
_DNSEarleyRHSName = _RHSName.unConstName._2

projectDNSEarleyFunc = \case
 LeafRHS{} -> Nothing 
 TreeRHS pRHS gRHS -> Just (pRHS, gRHS) 

