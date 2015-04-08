{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
module Commands.Servers.Servant.API where
import           Commands.Servers.Servant.Types

import           Control.Monad.Trans.Either     (EitherT)
import           Network.Wai                    (Application)
import           Network.Wai.Handler.Warp       (Port, run)
import           Servant

import           Control.Monad.IO.Class         (liftIO)
import qualified Data.Map                       as Map


interpret :: [String] -> IO ()
interpret words = print words

response_Nothing, response_Just :: DGNUpdate String
response_Nothing = DGNUpdate Nothing Nothing Nothing
response_Just = DGNUpdate (Just "rule") (Just ["export one", "export two"]) (Just $ Map.fromList [("list one", ["a","b","c"]),("list two", ["a","b","c"])])


type Response a = EitherT (Int, String) IO a

{- | POST /recognition

@
$ curl -d '["some","words"]' 'http://localhost:8666/recognition/'
$ python -c 'import urllib2,json,sys; print json.loads(urllib2.urlopen("http://localhost:8666/recognition/", json.dumps(["some","words with spaces"])).readline())'
@


-}
type CommandsAPI = "recognition" :> ReqBody Recognition :> Post (DGNUpdate String)

commandsAPI :: Proxy CommandsAPI
commandsAPI = Proxy

commandsServer :: Server CommandsAPI
commandsServer = postWords
 where
 postWords :: Recognition -> Response (DGNUpdate String)
 postWords (Recognition words) = do
  liftIO $ interpret words
  return $ case words of
   [] -> response_Nothing
   _ -> response_Just


commandsApplication :: Application
commandsApplication = serve commandsAPI commandsServer

serveCommands :: Port -> IO ()
serveCommands port = run port commandsApplication

