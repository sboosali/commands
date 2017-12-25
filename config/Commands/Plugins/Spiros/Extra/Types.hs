{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
module Commands.Plugins.Spiros.Extra.Types 
 ( Generic, Data, NFData, Semigroup, Monoid
 , Ordinal(..), Number  
 ) where 

import Control.DeepSeq (NFData) 
import Data.Semigroup (Semigroup) 

import GHC.Generics(Generic) 
import Data.Data(Data)


type Number = Int               -- TODO 

-- | an ordinal number. i.e. "first", "second", "third", "fourth", et cetera  
newtype Ordinal = Ordinal { unOrdinal :: Integer }  -- NOTE a GeneralizedNewtypeDeriving Show, doesn't show field accessories in constructor
 deriving (Show,Read,Eq,Ord,Enum,Num,Integral,Real,Generic,Data,NFData)

