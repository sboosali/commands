

-- |
module Commands.Servers.Simple
 ( module Commands.Servers.Simple
 , module Commands.Servers.Simple.Types
 ) where
import Commands.Servers.Simple.Types

import qualified Workflow.Core as W

import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Wai
import Servant

import Control.Monad

import Prelude.Spiros
import Prelude()

{-|

e.g.

@
import Workflow.<platform>
runSimpleServer $ defaultSettings runWorkflowT
@

-}
main = do
  print "Commands.Servers.Simple"
  -- runSimpleServer (defaultSettings ?) 

{- | launches the server.

Simultaneously, reads from standard input, for debugging.

-}
runSimpleServer :: Settings -> IO ()
runSimpleServer settings@Settings{..} = do
	forkever_ $do
		getLine >>= cmdln
	Wai.run port (serve recognitionAPI (handlers settings))

handlers :: Settings -> Server RecognitionAPI
handlers = handleRecognition
-- type family ServerT (layout :: k) (m :: * -> *) :: *

handleRecognition :: Settings -> RecognitionHandler
handleRecognition Settings{handle, exec= W.ExecuteWorkflow exec} ws
 = Handler $ liftIO $ exec $ handle ws

-- _500 :: String -> ServantErr _500 s = err500{ errBody = BS.pack e }
