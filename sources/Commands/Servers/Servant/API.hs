{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
module Commands.Servers.Servant.API where
import Commands.Servers.Servant.Types

import Control.Monad.Trans.Either     (EitherT)
import Network.Wai                    (Application)
import Network.Wai.Handler.Warp       (Port, run)
import Servant

import Control.Monad.IO.Class         (liftIO)


interpret :: Recognition -> IO ()
interpret (Recognition words) = print words

type Response a = EitherT (Int, String) IO a

{- | POST /recognition

@
$ curl -d '["some","words"]' 'http://localhost:8666/recognition/'
@

-}
type CommandsAPI = "recognition" :> ReqBody Recognition :> Post (DGNUpdate String)

commandsAPI :: Proxy CommandsAPI
commandsAPI = Proxy

commandsServer :: Server CommandsAPI
commandsServer = postWords
 where
 postWords :: Recognition -> Response (DGNUpdate String)
 postWords words = do
  liftIO $ interpret words
  return $ DGNUpdate Nothing Nothing Nothing

commandsApplication :: Application
commandsApplication = serve commandsAPI commandsServer

serveCommands :: Port -> IO ()
serveCommands port = run port commandsApplication

