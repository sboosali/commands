{-# LANGUAGE DeriveAnyClass, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Plugins.Spiros.Shortcut.Types where
import Commands.Plugins.Spiros.Extra.Types 
import Commands.Plugins.Spiros.Extra

-- import Commands.Extra
import Commands.Mixins.DNS13OSX9
import Commands.Backends.Workflow

import           Data.Text.Lazy                 (Text)

import           GHC.Exts                        (IsString (..))


newtype Shortcut = Shortcut KeySequence
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData)

runShortcut (Shortcut ks) = sendKeySequence ks

shortcuts :: (Functor'RHS n Text f) => [(String,String)] -> RHS n Text f Shortcut
shortcuts
 = fmap Shortcut
 . foldMap (\(s,k) -> readEmacsKeyChord k & maybe (__error__ k) (<$ fromString s))
 . filterBlanks
 where
 __error__ k = error $ "ERROR Commands.Plugins.Spiros.Shortcut.Types.shortcuts " ++ show k --TODO CallStack
 -- fail loudly

