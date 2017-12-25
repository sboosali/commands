{-| enrich Commands.Plugins.Spiros.Types with 'SpirosType'.

-}
module Commands.Plugins.Spiros.Module
 ( module Commands.Plugins.Spiros.Module
 , module Commands.Plugins.Spiros.Types
 ) where
import Commands.Plugins.Spiros.Types
import Commands.Plugins.Spiros.Root.Types (Root)

import qualified Commands.Mixins.DNS13OSX9 as Dragon
--TODO import qualified Commands.Servers.Servant as Server 
--
--
-- type SpirosResponse    = SpirosV Server.DNSResponse
--
-- type SpirosV           = Server.V SpirosBackend SpirosContext SpirosType
--
-- type SpirosHandler   i = Server.VHandler SpirosBackend SpirosContext SpirosType i
--
-- type SpirosSettings    = Server.VSettings SpirosBackend SpirosContext SpirosType
--
-- type SpirosEnvironment = Server.VEnvironment SpirosBackend SpirosContext SpirosType
--
-- type SpirosPlugin      = Server.VPlugin SpirosBackend SpirosContext SpirosType

type SpirosCommand     = Dragon.DNSEarleyCommand SpirosContext SpirosMonad_ SpirosType

type SpirosType        = Root -- or Roots
