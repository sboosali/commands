{-# LANGUAGE DataKinds, RankNTypes, TupleSections, TypeFamilies #-}
{-# LANGUAGE TypeOperators                                      #-}
module Commands.Servers.Servant.API where
import Commands.Backends.OSX          hiding (Application, Command)
import Commands.Core
import Commands.Mixins.DNS13OSX9
import Commands.Servers.Servant.Types

import Control.Lens
import Control.Monad.Trans.Either     (EitherT)
import Network.Wai                    (Application)
import Network.Wai.Handler.Warp       (Port, run)
import Servant

import Control.Monad.IO.Class         (liftIO)


-- makeServantInterpreter :: -> Interpreter
-- makeServantInterpreter

type Response = EitherT ServantErr IO

type CmdModelZ a = forall z. CmdModel z a

{- | POST /recognition

@
$ export PORT=8666
$ curl -d '["some","words"]' 'http://localhost:$PORT/recognition/'
$ python -c 'import urllib2,json,sys; print json.loads(urllib2.urlopen("http://localhost:$PORT/recognition/", json.dumps(["some","words with spaces"])).readline())'
@


-}
type NatlinkAPI = "recognition" :> ReqBody '[JSON] DGNRecognition :> Post '[JSON] ()
-- type NatlinkAPI = "recognition" :> ReqBody DGNRecognition :> Post (DGNUpdate String)

natlinkAPI :: Proxy NatlinkAPI
natlinkAPI = Proxy

natlinkHandlers :: (Show a) => CmdModelZ a -> Server NatlinkAPI
natlinkHandlers = postRecognition

postRecognition :: (Show a) => CmdModelZ a -> DGNRecognition -> Response ()
postRecognition cm (DGNRecognition ws) = handleInterpret cm $ unwords ws

{- | this handler:

* supports timeout (with TODO). the parsers are fast, even when backtracking exponentially, as it's Haskell and the sentences are short. however, the grammars can be recursively defined, which could result in on termination.
* supports short-circuiting (in 'EitherT'), returning an HTTP error status.
* executes the compiled actions (in 'IO').


-}
handleInterpret :: (Show a) => CmdModelZ a -> String -> Response ()
handleInterpret cm s = do
 liftIO $ putStrLn s
 let v = either (const $ (_modDefault cm) s) id $ ((_modCommand cm)^.comRule) `parses` s
 liftIO $ print v
 let as = ((_modCommand cm) `compiles` v) (_modContext cm)
 liftIO $ putStrLn $ showActions as
 liftIO $ runActions as
 return ()

-- handleInterpret :: (Show a) => CmdModelZ a -> String -> Response ()
-- handleInterpret (CmdModel command def context) text = do
--
-- can't bind the rank2 field to command:
--
-- Couldn't match type ‘z0’ with ‘z’
--       because type variable ‘z’ would escape its scope
--     This (rigid, skolem) type variable is bound by
--       a type expected by the context: R z a
--       at sources/Commands/Servers/Servant/API.hs:33:41-72
--     Expected type: C z ApplicationDesugarer Actions_ a
--       Actual type: C z0 ApplicationDesugarer Actions_ a

natlinkApplication :: (Show a) => CmdModelZ a -> Application
natlinkApplication cm = serve natlinkAPI $ natlinkHandlers cm

serveNatlink :: (Show a) => Port -> CmdModelZ a -> IO ()
serveNatlink port cm = run port $ natlinkApplication cm
-- I think is relevant that ($) is specially typed
-- point-free doesn't work:
--
-- serveNatlink :: (Show a) => Port -> CmdModelZ a -> IO ()
-- serveNatlink port = run port . natlinkApplication
    -- Cannot instantiate unification variable ‘a0’
    -- with a type involving foralls:
    --   forall (z :: * -> * -> * -> *). CmdModel z a
    --   Perhaps you want ImpredicativeTypes
