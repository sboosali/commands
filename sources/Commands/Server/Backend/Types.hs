{-# LANGUAGE RecordWildCards, RankNTypes, TemplateHaskell #-}

{-|
-}
module Commands.Server.Backend.Types where
import Commands.Server.Correction.Types
import Commands.Server.Platform.Types
import Commands.Server.Correction.Shell

import qualified           Commands.Frontends.Dragon13 as DNS
import Data.Address
import Workflow.Core (MonadWorkflow,WorkflowT,ExecuteWorkflow)
import qualified Workflow.Core as W

import Control.Lens (makeLenses)

import Prelude.Spiros
import Prelude()
{-|
-}

{-| Platform-specific capabilities the server requires.

-}
data Backend m = Backend
 { _bExecuteWorkflow    :: ExecuteWorkflow -- Workflow :~> IO
 , _bCorrectionSettings :: CorrectionSettings m -- ^ Should be at least (MonadIO m, MonadWorkflow m)
 , _bReachLoggingUi      :: m ()
 }

{-|

e.g.

@
defaultBackend _ ::Backend (WorkflowT IO)
@

-}
defaultBackend :: (MonadIO m, MonadWorkflow m) => ExecuteWorkflow -> Backend m
defaultBackend _bExecuteWorkflow = Backend{..}
 where
 _bCorrectionSettings = defaultCorrectionSettings
 _bReachLoggingUi      = reachShell

--------------------------------------------------------------------------------

{-| Platform-independent settings for the server.

-}
data Settings = Settings
 { _sNoise                :: [String]
 , _sPort                 :: Port
 , _sUIAddress            :: Address
 , _sNatLinkConfig        :: DNS.NatLinkConfig
 }

defaultSettings :: Settings -- (MonadIO m, MonadWorkflow m) => Settings m
defaultSettings = Settings{..}
 where
 _sNoise                = defaultNoise
 _sPort                 = defaultPort
 _sUIAddress            = defaultUIAddress
 _sNatLinkConfig        = defaultNatLinkConfig

defaultNoise = ["the","will","if","him","A","that","a","she","and"
  ]

defaultPort = Port 8888

defaultUIAddress = Address localhost (Port 8889)  -- TODO Just

defaultNatLinkConfig = DNS.NatLinkConfig -- TODO what the heck is this stuff
  (Address default_VirtualBox_HostOnlyNetwork_Host defaultPort)
  "E:/commands/log.txt"
  "E:/commands/context.json"

{-|

@= "192.168.56.1"@

-}
default_VirtualBox_HostOnlyNetwork_Host :: Host
default_VirtualBox_HostOnlyNetwork_Host = Host "192.168.56.1"

--------------------------------------------------------------------------------

makeLenses ''Backend
makeLenses ''Settings
