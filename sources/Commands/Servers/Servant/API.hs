{-# LANGUAGE DataKinds, TupleSections, TypeFamilies, TypeOperators #-}
module Commands.Servers.Servant.API where
import Commands.Backends.OSX          hiding (Application, Command)
import Commands.Core
import Commands.Servers.Servant.Types

import Control.Lens
import Control.Monad.Trans.Either     (EitherT)
import Network.Wai                    (Application)
import Network.Wai.Handler.Warp       (Port, run)
import Servant

import Control.Monad.IO.Class         (liftIO)
-- import qualified Data.Map                       as Map


-- makeInterpreter :: -> Interpreter
-- makeInterpreter


{- | this handler:

* supports timeout (with TODO). the parsers are fast, even when backtracking exponentially, as it's Haskell and the sentences are short. however, the grammars can be recursively defined, which could result in on termination.
* supports short-circuiting (in 'EitherT'), returning an HTTP error status.
* executes the compiled actions (in 'IO').


-}
handleInterpret :: (Show v) => CModel v -> String -> Response ()
handleInterpret (CModel command def context) text = do
 liftIO $ putStrLn text
 let v = parse command text
 liftIO $ print v
 let as = (command `compiles` v) context
 liftIO $ putStrLn $ showActions as
 liftIO $ runActions as
 return ()
 where
 parse c s = either (const $ def s) id $ (c ^. comGrammar) `parses` s

-- handleInterpret :: CModel a -> String -> Response ()
-- handleInterpret (CModel _c _ax) s = do
--  liftIO $ putStrLn s
--  let p = c ^. comGrammar
--  let failure _e = do
--   liftIO $ print s
--   left (400, ())
--  let success x = do
--   liftIO $ print $ showActions ((c `compiles` x) context)
--   right ()
--  let result = p `parses` s
--  mapEitherT failure success result

-- attemptInterpret c context s = do
--  let p = c ^. comGrammar
--  x <- mapEitherT (\e -> (400, show e)) id $ p `parses` s
--  let a = (c `compiles` x) cx
--  liftIO $ print $ showActions a
--  return $ (200, ())

 -- where
 -- onFailure = (\e -> (400, show e))
 -- onSuccess = (\x -> (200, showActions $ (c `compiles` x) context))
 -- result = p `parses` s
 -- mapEitherT onFailure onSuccess result
 -- hoistEither


type Response = EitherT (Int, String) IO

{- | POST /recognition

@
$ export PORT=8666
$ curl -d '["some","words"]' 'http://localhost:$PORT/recognition/'
$ python -c 'import urllib2,json,sys; print json.loads(urllib2.urlopen("http://localhost:$PORT/recognition/", json.dumps(["some","words with spaces"])).readline())'
@


-}
type CommandsAPI = "recognition" :> ReqBody DGNRecognition :> Post ()
-- type CommandsAPI = "recognition" :> ReqBody DGNRecognition :> Post (DGNUpdate String)

commandsAPI :: Proxy CommandsAPI
commandsAPI = Proxy

commandsServer :: (Show a) => CModel a -> Server CommandsAPI
commandsServer = postRecognition

postRecognition :: (Show a) => CModel a -> DGNRecognition -> Response ()
postRecognition cm (DGNRecognition ws) = handleInterpret cm $ unwords ws

commandsApplication :: (Show a) => CModel a -> Application
commandsApplication = serve commandsAPI . commandsServer

serveCommands :: (Show a) => Port -> CModel a -> IO ()
serveCommands port = run port . commandsApplication

