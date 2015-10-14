{-# LANGUAGE LambdaCase, RankNTypes, RecordWildCards, TupleSections #-}
{-# LANGUAGE TypeFamilies                                           #-}
module Commands.Servers.Servant.API where
import           Commands.Mixins.DNS13OSX9
import           Commands.Servers.Servant.Types

import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Wai
import           Servant



serveNatlink :: (Show a) => (forall r. RULED (VSettings m) r a) -> IO ()
serveNatlink settings@VSettings{..} = do
 vSetup settings >>= \case
  Left e  -> do
   print e
  Right() -> do
   Wai.run vPort (natlinkApplication settings)

-- makeServantInterpreter :: -> Interpreter
-- makeServantInterpreter

natlinkApplication :: (Show a) => (forall r. RULED (VSettings m) r a) -> Wai.Application
natlinkApplication vSettings = serve natlinkAPI (natlinkHandlers vSettings)

natlinkAPI :: Proxy NatlinkAPI
natlinkAPI = Proxy

natlinkHandlers :: (Show a) => (forall r. RULED (VSettings m) r a) -> Server NatlinkAPI
natlinkHandlers = postRecognition

postRecognition :: (Show a) => (forall r. RULED (VSettings m) r a) -> DGNRecognition -> Response ()
-- postRecognition vSettings (DGNRecognition ws) = (vSettings&vInterpretRecognition) vSettings ws
postRecognition vSettings (DGNRecognition ws) = (vInterpretRecognition vSettings) vSettings ws
