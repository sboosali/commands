{-# LANGUAGE DeriveAnyClass, StandaloneDeriving, LambdaCase #-}
module Commands.Plugins.Spiros.Root.Types where
import           Commands.Plugins.Spiros.Extra.Types
import           Commands.Plugins.Spiros.Act.Types
import           Commands.Plugins.Spiros.Macros.Types
import           Commands.Plugins.Spiros.Emacs.Types
import           Commands.Plugins.Spiros.Shell.Types
import           Commands.Plugins.Spiros.Shortcut.Types
import           Commands.Plugins.Spiros.Phrase.Types

import Control.DeepSeq (NFData(..), deepseq)


data Root
 -- = Frozen [Stage] Root
 -- | Ambiguous Root
 = Macro_      Number  Macro  -- ^ repeated
 | Root_ Root_
 deriving (Show,Eq,Ord)
instance NFData Root where     -- TODO complete the instance, we only care about Root_ for now
 rnf = \case
  -- Frozen{} -> ()
  -- Ambiguous{} -> ()
  Macro_ n m -> n `deepseq` m `deepseq` ()
  Root_ r -> r `deepseq` ()

-- -- | the stages of the DSL
-- data Stage = RawStage | ParseStage | RunStage deriving (Show,Read,Eq,Ord,Bounded,Enum,Generic,Data,NFData)

data Root_
 = Acts_       [Acts]         -- ^ chained and repeated
 | Shortcut_   Number  Shortcut  -- ^ repeated
 | Shell_              Shell  -- ^
 | Emacs_      Number  Emacs  -- ^ repeated
 | Letters_    Letters       -- ^
 | Dictation_  Dictation     -- ^
 | Phrase_     Phrase        -- ^
 -- TODO | Click_ Click
 deriving (Show,Eq,Ord,Generic,Data,NFData)  -- TODO no instance for ,Generic,Data, because Macro is a GADT

data Click = Click Times Button deriving (Show,Read,Eq,Ord,Generic,Data,NFData)

data Times = Single | Double | Triple deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData)

data Button = LeftButton | MiddleButton | RightButton deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData)
