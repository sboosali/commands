{-# LANGUAGE LambdaCase, RankNTypes, RecordWildCards, TupleSections #-}
{-# LANGUAGE TypeFamilies                                           #-}
module Commands.Servers.Servant.API where
import           Commands.Extra 
import           Commands.Mixins.DNS13OSX9
import           Commands.Servers.Servant.Types

import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Wai
import           Servant
import           Servant.Client (client)


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

natlinkHandlers :: (Show a) => (forall r. RULED (VSettings m) r a) -> Server NatlinkAPI
natlinkHandlers vSettings = postRecognition vSettings :<|> postHypotheses vSettings :<|> postCorrection vSettings -- TODO ReaderT

postRecognition :: (Show a) => (forall r. RULED (VSettings m) r a) -> RecognitionRequest -> Response ()
-- postRecognition vSettings (RecognitionRequest ws) = (vSettings&vInterpretRecognition) vSettings ws
postRecognition vSettings = (vInterpretRecognition vSettings) vSettings 

{-| handle a hypothesis request, as a server  

-}
postHypotheses :: (Show a) => (forall r. RULED (VSettings m) r a) -> HypothesesRequest -> Response ()
postHypotheses vSettings = (vInterpretHypotheses vSettings) vSettings 

postCorrection :: (Show a) => (forall r. RULED (VSettings m) r a) -> CorrectionRequest -> Response () 
postCorrection vSettings = (vInterpretCorrection vSettings) vSettings 

{-| forward a hypothesis request, as a client  

-}
postHypothesesTo :: Address -> HypothesesRequest -> ClientResponse
postHypothesesTo address = client hypothesesAPI (address2baseurl address) 

