module DragonNaturallySpeaking.Extra
 ( module DragonNaturallySpeaking.Extra
 , module X
 ) where

import Control.DeepSeq as X (NFData)
import Data.Hashable as X (Hashable)
import Data.Semigroup as X (Semigroup)

import GHC.Generics as X (Generic)
import Data.Data as X (Data)

import Control.Arrow as X ((>>>))
import Data.Function as X ((&))
import Data.Foldable as X (traverse_)

nothing :: (Monad m) => m ()
nothing = return ()

maybe2bool :: Maybe a -> Bool
maybe2bool = maybe False (const True)

either2maybe :: Either e a -> Maybe a
either2maybe = either (const Nothing) Just

either2bool :: Either e a -> Bool
either2bool = either (const False) (const True)

