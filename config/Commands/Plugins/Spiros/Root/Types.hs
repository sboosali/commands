{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, LambdaCase  #-}
module Commands.Plugins.Spiros.Root.Types where
import           Commands.Plugins.Spiros.Etc
import           Commands.Plugins.Spiros.Emacs
import           Commands.Plugins.Spiros.Macros.Types 
import           Commands.Plugins.Spiros.Shell
import           Commands.Plugins.Spiros.Edit
import           Commands.Plugins.Spiros.Shortcut.Types 
import           Commands.Plugins.Spiros.Phrase.Types 

import Commands.Backends.OSX (KeyRiff) 


data Roots
 = Frozen Root
 | Ambiguous Root
 | Root_ Root
 deriving (Show,Eq)

data Root
 = Acts_       [Acts]         -- ^ chained and repeated
 | Macro_      Number  Macro  -- ^ repeated 
 | Shortcut_   Number  Shortcut  -- ^ repeated 
 | Shell_              Shell  -- ^
 | Emacs_      Number  Emacs  -- ^ repeated 
 | Dictation_  Dictation      -- ^ 
 | Phrase_     Phrase        -- ^ 
 deriving (Show,Eq)

data Acts
 = ActsRW Int Act   -- ^ read/write actions
 deriving (Show,Read,Eq)

data Act
 = KeyRiff_ KeyRiff
 --TODO | Click_ Click
 | Edit_ Edit
 | Move_ Move
 deriving (Show,Read,Eq)

data Click = Click Times Button deriving (Show,Read,Eq,Ord)

data Times = Single | Double | Triple deriving (Show,Read,Eq,Ord,Enum,Bounded)

data Button = LeftButton | MiddleButton | RightButton deriving (Show,Read,Eq,Ord,Enum,Bounded)

