{-# LANGUAGE FlexibleContexts #-}
module Commands.Plugins.Spiros.Shortcut.Types where

import Commands.Extra
import Commands.Mixins.DNS13OSX9
import Commands.Backends.OSX

import           Data.Text.Lazy                 (Text)

import           GHC.Exts                        (IsString (..))


newtype Shortcut = Shortcut KeyRiff
 deriving (Show,Read,Eq,Ord)

runShortcut :: MonadWorkflow m => Shortcut -> m ()                   
runShortcut (Shortcut kr) = runKeyRiff kr

shortcuts :: (Functor'RHS n Text f) => [(String,String)] -> RHS n Text f Shortcut
shortcuts
 = fmap Shortcut
 . foldMap (\(s,k) -> kbd k <$ fromString s)
 . filterBlanks

