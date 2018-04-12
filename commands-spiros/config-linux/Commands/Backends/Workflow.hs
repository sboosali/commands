-- | saves client from directly depending on `workflow-types`, `workflow-osx` and (possibly) `exceptions`
module Commands.Backends.Workflow
 ( module Workflow.Core
 , module Workflow.Derived
 , module PlatformSpecificWorkflow
 , module Control.Monad.Catch
 , module Commands.Backends.Workflow
 ) where
import Workflow.Core
import Workflow.Derived


--import "workflow-x11-shell" Workflow.X11.Execute as PlatformSpecificWorkflow
import "workflow-x11-shell" Workflow.Backends.X11 as PlatformSpecificWorkflow

import Control.Monad.Catch (MonadThrow)

type ClipboardText = Clipboard --TODO
