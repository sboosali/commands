{-# LANGUAGE LambdaCase, RankNTypes, RecordWildCards, TupleSections #-}
{-# LANGUAGE TypeFamilies                                           #-}
module Commands.Servers.Servant.API where
import qualified Commands.Backends.OSX          as OSX
import           Commands.Mixins.DNS13OSX9
import           Commands.Servers.Servant.Types
-- import           Commands.Core

import           Control.Lens
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy.Char8     as BSC
import           Data.Text.Lazy                 (Text)
import qualified Data.Text.Lazy                 as T
import qualified Data.Text.Lazy.IO              as T
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Wai
import           Servant

import           Control.Monad.IO.Class         (liftIO)


serveNatlink :: (Show a) => (forall r. VSettings_ r a) -> IO ()
serveNatlink settings@VSettings{..} = do
 vSetup settings >>= \case
  Left e  -> do
   print e
  Right() -> do
   Wai.run vPort $ natlinkApplication settings

-- makeServantInterpreter :: -> Interpreter
-- makeServantInterpreter

natlinkApplication :: (Show a) => (forall r. VSettings_ r a) -> Wai.Application
natlinkApplication vSettings = serve natlinkAPI $ natlinkHandlers vSettings

natlinkAPI :: Proxy NatlinkAPI
natlinkAPI = Proxy

natlinkHandlers :: (Show a) => (forall r. VSettings_ r a) -> Server NatlinkAPI
natlinkHandlers = postRecognition

postRecognition :: (Show a) => (forall r. VSettings_ r a) -> DGNRecognition -> Response ()
postRecognition vSettings (DGNRecognition ws) = handleInterpret vSettings ws

{- | this handler:

* supports timeout (with TODO). the parsers are fast, even when backtracking exponentially, as it's Haskell and the sentences are short. however, the grammars can be recursively defined, which could result in on termination.
* supports short-circuiting (in 'EitherT'), returning an HTTP error status.
* executes the compiled actions (in 'IO').


-}
handleInterpret :: (Show a) => (forall r. VSettings_ r a) -> [Text] -> Response ()
handleInterpret vSettings = \ts -> do
-- handleInterpret vSettings@VSettings{vConfig=VConfig{..},..} = \ts -> do
-- handleInterpret :: (Show a) => VSettings z a -> (VConfig z a) -> [Text] -> Response ()
-- handleInterpret VSettings{..} VConfig{..} = \ts -> do
 context <- liftIO$ (vSettings&vExecuteActions) OSX.currentApplication

 liftIO$ putStrLn "" >> putStrLn "" >> putStrLn ""
 liftIO$ putStrLn ""
 liftIO$ T.putStrLn$ T.intercalate (T.pack " ") ts
 vals <- case e'ParseBest (vSettings&vConfig&vParser) ts of
  Left  e -> left  err400{errBody = BSC.pack (show e)}
  Right v -> right v
 liftIO$ print vals
 let acts = (vSettings&vConfig&vDesugar) context vals
 liftIO$ putStrLn $ OSX.showActions acts
 liftIO$ (vSettings&vExecuteActions) acts
 return ()

