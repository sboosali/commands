

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

runSimpleServer :: Settings -> IO ()
runSimpleServer settings@Settings{..} = Wai.run port (serve recognitionAPI (handlers settings))

handlers :: Settings -> Server RecognitionAPI
handlers = handleRecognition
-- type family ServerT (layout :: k) (m :: * -> *) :: *

handleRecognition :: Settings -> RecognitionHandler
handleRecognition Settings{handle, exec= W.ExecuteWorkflow exec} ws
 = liftIO $ exec $ handle ws

-- _500 :: String -> ServantErr _500 s = err500{ errBody = BS.pack e }
