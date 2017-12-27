

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
import Control.Concurrent (forkIO)

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
  _read cmdln
  _serve
  where
  _read = \case
    Nothing     -> nothing
    Just cmdln' -> forkever_ $ getLine >>= cmdln'
  _serve = Wai.run port (serve recognitionAPI (handlers settings))

handlers :: Settings -> Server RecognitionAPI
handlers settings = handleRecognition settings :<|> handleTest
-- type family ServerT (layout :: k) (m :: * -> *) :: *

handleRecognition :: Settings -> RecognitionHandler
handleRecognition Settings{handle,exec} ws
 = Handler (go ws & forkIO & void & liftIO) -- TODO fork necessary?  Probably not, server spins off new thread per req
-- _500 :: String -> ServantErr _500 s = err500{ errBody = BS.pack e }
 where
 go = handle exec

handleTest :: Response String
handleTest = return "success (commands-server-simple)"
