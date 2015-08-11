{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE LambdaCase, RankNTypes, RecordWildCards, TypeOperators  #-}
module Commands.Servers.Servant.Types where
import qualified Commands.Backends.OSX                 as OSX
-- import           Commands.Etc
import qualified Commands.Frontends.Dragon13.Serialize as DNS
import           Commands.Mixins.DNS13OSX9             (EarleyParser, RULED)

import           Control.Monad.Trans.Either            (EitherT)
import           Data.Aeson
import           Data.Text.Lazy                        (Text)
import qualified Network.Wai.Handler.Warp              as Wai
import           Servant

import           GHC.Generics                          (Generic)


type Response = EitherT ServantErr IO

{- |

@
POST /recognition
@

e.g.

@
$ export PORT=8666
$ curl  -X POST  -H "Content-Type: application/json"  -d '["some","words"]'  "http://localhost:$PORT/recognition/"
$ python -c 'import sys,os,json,urllib2; print (urllib2.urlopen(urllib2.Request("http://localhost:"+os.environ["PORT"]+"/recognition/", json.dumps(["some","words with spaces"]), {"Content-Type": "application/json"})).readline())'
@

-}
type NatlinkAPI = "recognition" :> ReqBody '[JSON] DGNRecognition :> Post '[JSON] ()
-- type NatlinkAPI = "recognition" :> ReqBody DGNRecognition :> Post (DGNUpdate String)

{- |
-}
newtype DGNRecognition = DGNRecognition [Text]  deriving (Show,Eq,Ord,Generic,FromJSON)

{- | read-only.
"static" configuration
-}
data VSettings z a = VSettings
 { vPort                 :: Wai.Port
 , vSetup                :: VSettings z a -> IO (Either VError ())
 , vInterpretRecognition :: (forall r. RULED VSettings r a) -> [Text] -> EitherT ServantErr IO ()
 , vConfig               :: VConfig z a
 -- , vUpdateConfig   :: VPlugin z :~>: VConfig z
 }

{- |
-}
data VError = VError String
 deriving (Show,Eq,Ord)

{- | read-only.
"dynamic" configuration
-}
data VConfig z a = VConfig
 { vGrammar :: DNS.SerializedGrammar
 , vParser  :: EarleyParser z a
 , vDesugar :: OSX.Application -> a -> OSX.Actions_
 }

