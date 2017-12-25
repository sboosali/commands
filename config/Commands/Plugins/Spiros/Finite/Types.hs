{-# LANGUAGE DeriveAnyClass #-}
module Commands.Plugins.Spiros.Finite.Types where
import           Commands.Plugins.Spiros.Extra.Types 
import           Commands.Plugins.Spiros.Edit.Types

import Numeric.Natural


data Finite
 = Finite Natural Finite0
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData)

data Finite0
 = Edit0 Edit
 | Move0 Move
 -- | KeyRiff_ KeySequence
 --TODO | Click_ Click
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData)
