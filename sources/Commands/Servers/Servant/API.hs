{-# LANGUAGE LambdaCase, RankNTypes, RecordWildCards, TupleSections #-}
{-# LANGUAGE TypeFamilies, TypeOperators                                            #-}
module Commands.Servers.Servant.API where
import           Commands.Extra 
import           Commands.Servers.Servant.Types
import           Commands.Servers.Servant.V 
import           Commands.Servers.Servant.API.Types 
import Data.HTypes ((:~>)) 

import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Wai
import           Servant hiding((:~>)) 
import           Servant.Client (client)
import qualified Data.ByteString.Lazy.Char8    as BS
import           Control.Lens 

import           Control.Monad.Trans.Either
import           Control.Monad.Reader 
-- import           Control.Monad.IO.Class        (liftIO)
-- import Control.Concurrent.STM
-- import Data.Function ((&)) 


natlinkIO :: VSettings m c v -> IO ()
natlinkIO settings = do
 environment <- getVEnvironment settings 
 (settings&vSetup) environment >>= \case

  Left e -> do
      print e

  Right () -> do
      Wai.run (getVPort settings) (natlinkApplication settings)

-- makeServantInterpreter :: -> Interpreter
-- makeServantInterpreter

natlinkApplication :: VSettings m c v -> Wai.Application 
natlinkApplication settings = serve natlinkAPI (natlinkServer settings) 

-- TODO type IsValue v = (Show v) 

natlinkServer :: VSettings m c v -> Server NatlinkAPI 
natlinkServer settings = enterV (runVServant settings) (natlinkHandlers settings) 

enterV
 :: (V m c v :~> EitherT ServantErr IO)
 -> (ServerT NatlinkAPI (V m c v) -> ServerT NatlinkAPI (EitherT ServantErr IO))
 -- -> (forall x. ServerT NatlinkAPI (V m c v x) -> ServerT NatlinkAPI (EitherT ServantErr IO x))
 -- -> (ServerT NatlinkAPI (V m c v) :~> ServerT NatlinkAPI (EitherT ServantErr IO))
enterV u = enter (Nat u) 

-- type family ServerT (layout :: k) (m :: * -> *) :: *

natlinkHandlers :: VSettings m c v -> ServerT NatlinkAPI (V m c v) 
natlinkHandlers VSettings{..} 
    = vInterpretRecognition
 :<|> vInterpretHypotheses
 :<|> vInterpretCorrection
 :<|> vInterpretReload 
 :<|> vInterpretContext 

-- | (use with 'enter') 
runVServant :: VSettings m c v -> (V m c v :~> EitherT ServantErr IO)
runVServant settings m = do 
 environment <- liftIO$ getVEnvironment settings 
 runVServantWith environment m 

-- | 
runVServantWith :: VEnvironment m c v -> (V m c v :~> EitherT ServantErr IO)
runVServantWith environment = runV >>> (flip runReaderT) environment >>> bimapEitherT errorV2Servant id 

errorV2Servant :: VError -> ServantErr
errorV2Servant (VError e) = err500{ errBody = BS.pack e } 

-- {-| handle a hypothesis request, as a server  
-- -}
-- postHypotheses :: (Show a) => (VSettings m c a) -> HypothesesRequest -> Response DNSResponse
-- postHypotheses vSettings = (vInterpretHypotheses vSettings) vSettings 

{-| forward a hypothesis request, as a client  

-}
postHypothesesTo :: Address -> HypothesesRequest -> ClientResponse
postHypothesesTo address = client hypothesesClientAPI (address2baseurl address) 

getVPort :: VSettings m c v -> Int
getVPort = view (to vConfig . to vSettings_ . to vPort . _Port)
-- getVPort settings = (settings&vConfig&vPort) ^. (_Port)
