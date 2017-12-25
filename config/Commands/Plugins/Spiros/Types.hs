{-# LANGUAGE TemplateHaskell, LambdaCase, GeneralizedNewtypeDeriving, ViewPatterns  #-}
module Commands.Plugins.Spiros.Types where
import Commands.Plugins.Spiros.Extra.Types

-- import qualified Commands.Servers.Servant as Server
-- import Commands.Backends.Workflow (WorkflowT,MonadWorkflow_,MonadThrow)
import           Commands.Parsers.Earley (EarleyParser)
import Workflow.Core (WorkflowT,MonadWorkflow_,MonadThrow)

-- import qualified System.FilePath.Posix as FilePath
import Control.Lens (makePrisms)
import Data.Text.Lazy (Text)

import Control.Monad.IO.Class (MonadIO)

import Prelude.Spiros (Default(..))


-- type SpirosConfig      = Server.VConfig SpirosBackend SpirosContext
-- TODO
-- type SpirosGlobals     = Server.VGlobals SpirosContext

type SpirosBackend     = SpirosMonad -- TODO

type SpirosMonad_      = SpirosMonad ()

type SpirosParser s r  = EarleyParser s r String Text

newtype SpirosMonad a = SpirosMonad
 { getSpirosMonad :: WorkflowT IO a
 } deriving
 ( Functor
 , Applicative
 , Monad

 , MonadIO
 , MonadThrow

 -- , Workflow.MonadWorkflow  -- can't derive ConstraintKinds
 , MonadWorkflow_

 -- , MonadNatlink
 -- , MonadVServer
 -- , MonadState Server.VState
 )

-- ================================================================ --

data SpirosContext
 = GlobalContext
 | EmacsContext
 | ChromeContext
 | IntelliJContext
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic)
instance NFData SpirosContext
instance Default SpirosContext where def = GlobalContext
-- instance IsString SpirosContext where fromString =  -- no, depends on a mapping, unlawful like fromaeson

makePrisms ''SpirosContext

-- ================================================================ --
