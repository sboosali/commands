{-# LANGUAGE TypeFamilies, KindSignatures, StandaloneDeriving, RankNTypes, TypeOperators, FlexibleContexts, LambdaCase #-}

{-| higher-order type-constructor fixed-point. 

see <http://www.timphilipwilliams.com/posts/2013-01-16-fixing-gadts.html>  

-}
module Data.HFunctor.Recursion where 
-- import Commands.Extra (Exists(..))
import Data.HPrelude

import Control.Category ((>>>), (<<<)) 
import Control.Monad((<=<), (>=>))
-- import qualified Data.Map as Map
-- import           Data.Map (Map)
-- import Control.Monad.Trans.State
-- import           Data.Function                   ((&))


-- ================================================================ --

{-| 

-}
type family HBase (h :: * -> *) :: (* -> *) -> (* -> *)


{-| 

e.g. 

@
instance HRecursive ('HFix' h) where hproject = unHFix
-- hproject :: (h0 ~ HFix h) => h0      :~>  ('HBase' h0)       h0
-- hproject ::                  HFix h  :~>  (HBase (HFix h)) (HFix h)
-- hproject ::                  HFix h  :~>  h                (HFix h)
@

-}
class HFunctor (HBase h) => HRecursive (h :: * -> *) where
  hproject :: h :~> (HBase h) h


{-| 

e.g. 

@
instance HCoRecursive ('HFix' h) where hinject = HFix
-- hinject :: (h0 ~ HFix h) => ('HBase' h0)       h0        :~>  h0
-- hinject ::                  (HBase (HFix h)) (HFix h)  :~>  HFix h 
-- hinject ::                  h                (HFix h)  :~>  HFix h
@

-}
class HFunctor (HBase h) => HCoRecursive (h :: * -> *) where 
  hinject :: (HBase h) h :~> h


-- ================================================================ --

{-| "higher-order algebra" 

-}
type HAlgebra h f = h f :~> f 


{-| 

e.g. expanding the type aliases: 

@
::   HAlgebra ExprF (K String)                     -> (          Expr         :~>  K String   )
:: (          ExprF (K String)   :~>  K String   ) -> (          HFix ExprF   :~>  K String   )
:: (forall x. ExprF (K String) x  -> (K String) x) -> (forall x. HFix ExprF x  -> (K String) x)
@


-}
hcata :: (HRecursive h, HFunctor (HBase h)) => HAlgebra (HBase h) f -> (h :~> f)
hcata algebra = hproject >>> hfmap (hcata algebra) >>> algebra 


{-| 

-}
hpara :: (HRecursive h, HFunctor (HBase h)) => ((HBase h) (h :*: f) :~> f) -> (h :~> f)
hpara algebra = hproject >>> hfmap (id .&&&. hpara algebra) >>> algebra 


-- ================================================================ --

{-| "higher-order algebra, monadic".  

-}
type HAlgebraM m h f = h f :~> (m :. f)

{-| 

-}
hcataM :: (Monad m, HRecursive h, HTraversable (HBase h)) => HAlgebraM m (HBase h) f -> (h :~> (m :. f))
hcataM algebra = (hproject >>> return) >=> htraverse (hcataM algebra) >=> algebra 


{-| 

-}
hparaM :: (Monad m, HRecursive h, HTraversable (HBase h)) => ((HBase h) (h :*: f) :~> (m :. f)) -> (h :~> (m :. f))
hparaM algebra = (hproject >>> return) >=> htraverse (return <&&&> hparaM algebra) >=> algebra 


-- ================================================================ --

{-| "higher-order coalgebra". 

-}
type HCoAlgebra h f = f :~> h f 


{-| 

-}
hana :: (HCoRecursive h, HFunctor (HBase h)) => (f :~> (HBase h) f) -> (f :~> h)
hana coalgebra = hinject <<< hfmap (hana coalgebra) <<< coalgebra


{-| 

-}
hapo :: (HCoRecursive h, HFunctor (HBase h)) => (f :~> (HBase h) (h :+: f)) -> (f :~> h)
hapo coalgebra = hinject <<< hfmap (id .|||. hapo coalgebra) <<< coalgebra


-- ================================================================ --

{-| "higher-order coalgebra, monadic".  

-}
type HCoAlgebraM m h f = f :~> (m :. h f)


{-| 

-}
hanaM
 :: ((h ~ HBase t), HCoRecursive t, Monad m, HTraversable h)
 => (f :~> (m :. h f))
 -> (f :~> (m :. t))
hanaM coalgebra = (return <<< hinject) <=< htraverse (hanaM coalgebra) <=< coalgebra


{-| 

-}
hapoM
 :: ((h ~ HBase t), HCoRecursive t, Monad m, HTraversable h)
 => (f :~> (m :. h (t :+: f)))
 -> (f :~> (m :. t))
hapoM coalgebra = (return <<< hinject) <=< htraverse (return <|||> hapoM coalgebra) <=< coalgebra


-- {-| 

-- e.g. 

-- @
-- hapoparaM
--  :: ((HFix h :*: f) :~> (IO :. h (HFix h :+: f)))
--  -> (f :~> (IO :. HFix h))
-- @ 

-- -}
-- hapoparaM
--  :: ((h ~ HBase t), HCoRecursive t, Monad m, HTraversable h)
--  => ((t :*: f) :~> (m :. h (t :+: f)))
--  -> (f         :~> (m :. t))
-- hapoparaM u = 

-- hapoparaM
--  :: ((h ~ HBase t), HCoRecursive t, HTraversable h, Monad m1, Monad m2)
--  => ((t :*: f) :~> (m1 :.       f))
--  -> (f         :~> (m2 :.       h (t :+: f)))
--  -> (f         :~> (m1 :. m2 :. t))

{-| 

e.g. 

@
hapoparaM'StableName'Search  
 :: ((h ~ HBase t), HCoRecursive t, HTraversable h, Monad m1, Monad m2)
 => ((t :*: h) :~> (IO    :.       h))
 -> (h         :~> (State :.       h (t :+: h)))
 -> (h         :~> (IO :. State :. t))
hapoparaM'StableName'Search = hapoparaM (getFirst >>> makeStableName) 
@


-}


-- ================================================================ --


{-| 

-}
-- hsearch
--  :: ( (m ~ State (Map k (Exists t))), Ord k
--     , (h ~ HBase t) , HTraversable h, HCoRecursive t
--     )
--  => (Exists t -> Maybe (k, Exists t)) -- Prism
--  -> (forall x. t x -> Map k (Exists t))
-- hsearch _getItem = flip(execState) Map.empty . hapoM coalgebra
--  where
--  coalgebra r = case _getItem r of
--   Nothing -> return $ Right r        -- neither seen nor unseen
--   Just (k,v) -> do
--       Map.lookup k & \case
--           Just r -> return $ Left r  -- had already been seen
--           Nothing -> do
--               Map.insert k v         -- 
--               return $ Right r       -- had not yet been seen


