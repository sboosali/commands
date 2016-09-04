{-# LANGUAGE TypeFamilies, KindSignatures, StandaloneDeriving, RankNTypes, TypeOperators, FlexibleContexts, LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-} 

{-| observed sharing.  

<http://www.ittc.ku.edu/~andygill/papers/reifyGraph.pdf> 

-}
module Data.HFunctor.ObserveSharing where 
--import Commands.Extra (forceStableName ) 
import Data.HPrelude
import Data.HFunctor.Recursion 
import Data.Some

import Data.Hashable 
--import qualified Data.HashMap.Lazy as Map 
import           Data.HashMap.Lazy (HashMap) 

--import Control.Monad.Trans.State
--import           Data.Function                   ((&))
import System.Mem.StableName


data SomeStableName = forall x. SomeStableName (StableName x)

instance Eq SomeStableName where 
 SomeStableName s1 == SomeStableName s2 = eqStableName s1 s2 

instance Hashable SomeStableName where 
  hashWithSalt salt (SomeStableName s) = hashWithSalt salt s 


{-| 'hsearch' where @k@ is a 'StableName' and @v@ is @t@.  

-}
hsearchByStableName 
 :: ( (h ~ HBase t) , HTraversable h, HCoRecursive t
    )
 => (forall x. t x -> Bool)     -- ^ True means we should track this case 
 -> (forall x. t x -> IO (HashMap (SomeStableName) (Some t)))
hsearchByStableName= undefined 
-- hsearchByStableName f = hsearch g
--  where
--  g x = if f x 
--   then do
--        s <- forceStableName x
--        return $ Just (SomeStableName s, Some x) 
--   else return Nothing 


{-| 

-}
hsearch
 :: ( Monad m , Eq k, Hashable k
    , (h ~ HBase t) , HTraversable h, HCoRecursive t
    )
 => (forall x. t x -> m (Maybe (k, v))) -- like a Prism. 
 -> (forall x. t x -> m (HashMap k v))
hsearch = undefined 

