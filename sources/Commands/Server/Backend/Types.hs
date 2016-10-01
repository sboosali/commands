{-# LANGUAGE RecordWildCards, RankNTypes #-}

{-|
-}
module Commands.Server.Backend.Types where
import Commands.Server.Correction.Types
import Commands.Server.Platform.Types
import Commands.Server.Correction.Shell

import Workflow.Core (MonadWorkflow,WorkflowT,ExecuteWorkflow)
import qualified Workflow.Core as W

import Prelude.Spiros
import Prelude()
{-|
-}

{-|
-}
data Backend m = Backend
 { _bExecuteWorkflow    :: ExecuteWorkflow -- Workflow :~> IO
 , _bNoise              :: [String]
 , _bCorrectionSettings :: CorrectionSettings m -- ^ Should be at least (MonadIO m, MonadWorkflow m)
 , _reachLoggingUi      :: m ()
 }

{-|
-}
defaultBackend :: (MonadIO m, MonadWorkflow m) => ExecuteWorkflow -> Backend m
defaultBackend _bExecuteWorkflow = Backend{..}
 where
 _bNoise              = ["the","will"]
 _bCorrectionSettings = defaultCorrectionSettings
 _reachLoggingUi      = reachShell
