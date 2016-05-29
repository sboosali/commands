{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{- |
-}
module Commands.Backends.OSX
 ( module Commands.Backends.OSX
 , module Workflow.OSX
 , module X
 ) where
import Workflow.OSX

import Workflow.Keys as X
import Workflow.Derived as X
import Workflow.Pure.Char as X

import Control.Monad.Free.Church  (F)

-- compat

digit2keypress = digit2keychord
int2keypress = int2keychord
-- keypress2char :: (MonadThrow m) => KeyChord -> m Char
keypress2char = keychord2char
-- char2keypress :: Char -> m0 KeyChord
char2keypress = char2keychord


-- | church-encoded
type CWorkflow = F WorkflowF
type CWorkflow_ = CWorkflow ()
