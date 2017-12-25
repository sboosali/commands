{-# LANGUAGE DeriveAnyClass #-}
module Commands.Plugins.Spiros.Act.Types where
import           Commands.Plugins.Spiros.Extra.Types 
import           Commands.Plugins.Spiros.Edit

import Commands.Backends.Workflow (KeySequence) 


data Acts
 = ActsRW Int Act   -- ^ repeated read/write actions
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData)

data Act
 = KeyRiff_ KeySequence
 --TODO | Click_ Click
 | Edit_ Edit
 | Move_ Move
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData)

{-old

data Acts
 = ActRW Int Act   -- ^ actions can be chained (RW means read/write) 
 | ActRO Act   -- ^ idempotent(ish) actions don't need immediate repetition (RO means read-in only) .

-}
