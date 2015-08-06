{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE LambdaCase, RankNTypes, RecordWildCards, TypeOperators  #-}
module Commands.Servers.Servant.Types where
import qualified Commands.Backends.OSX                 as OSX
import           Commands.Etc
import qualified Commands.Frontends.Dragon13.Serialize as DNS
import           Commands.Mixins.DNS13OSX9             (DNSEarleyCommand,
                                                        EarleyParser)

import           Control.Monad.Trans.Either            (EitherT)
import           Data.Aeson
import           Data.Text.Lazy                        (Text)
import qualified Network.Wai.Handler.Warp              as Wai
import           Servant
import qualified Text.Earley.Internal                  as E
-- import Data.Aeson.Types

import           GHC.Generics                          (Generic)
-- import Data.Map         (Map)


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

type VSettings_ r a = VSettings (E.Rule r a) a  --TODO can we make it polymorphic?
type VConfig_ r a = VConfig (E.Rule r a) a
type VPlugin_ r a = VPlugin (E.Rule r a) a

-- type VSettings_ a = forall r. VSettings (E.Rule r a) a  --TODO can we make it polymorphic?
-- type VConfig_ a = forall r. VConfig (E.Rule r a) a
-- type VPlugin_ a = forall r. VPlugin (E.Rule r a) a

{- | read-only.
"static" configuration
-}
data VSettings z a = VSettings
 { vPort           :: Wai.Port
 , vSetup          :: VSettings z a -> IO (Either VError ())
 , vTeardown       :: VSettings z a -> IO (Either VError ())
 , vExecuteActions :: (OSX.Actions :~>: IO)
 , vUpdateConfig   :: VPlugin z a -> IO (VConfig z a)
 -- , vUpdateConfig :: (forall x. VPlugin z x -> IO (VConfig z x))
 , vConfig         :: VConfig z a
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


{- |
can rebuild a 'VPlugin' with 'vReloadPlugin'.
-}
data VPlugin z a = VPlugin
 { vCommand :: DNSEarleyCommand z a
 }


{- | read-write.

state for the server's handlers, and may be updated by clients.

-}
data VState = VState
 { vApplication :: OSX.Application
 -- , vContext :: c
 }


{- | the default settings:

* port 1337
* no setup or teardown
* needs a 'vUpdateConfig', a 'vExecuteActions', a 'vPlugin'

-}
defSettings
 :: (OSX.Actions :~>: IO)
 -> (VPlugin z a -> IO (VConfig z a))
 -- -> (forall x. VPlugin z x -> IO (VConfig z x))
 -> (VPlugin z a)
 -> IO (VSettings z a)
defSettings vExecuteActions vUpdateConfig vPlugin = do
 vConfig <- vUpdateConfig vPlugin
 return VSettings{..}
 where
 vPort = 1337
 vSetup    _ = return$ Right()
 vTeardown _ = return$ Right()


{- |

>>> import qualified Data.ByteString.Lazy.Char8 as B
>>> (putStr . B.unpack . 'encode') (DGNUpdate Nothing Nothing Nothing :: DGNUpdate String)
{"dgnUpdateExports":null,"dgnUpdateRules":null,"dgnUpdateLists":null}
>>> (putStr . B.unpack . 'encode') $ DGNUpdate (Just "rule") (Just ["export_one", "export_two"]) (Just $ Map.fromList [("list_one", ["a","b","c"]),("list_two", ["a","b","c"])])
{"dgnUpdateExports":["export_one","export_two"],"dgnUpdateRules":"rule","dgnUpdateLists":{"list_two":["a","b","c"],"list_one":["a","b","c"]}}


should be a sum type, but it must be a silly product type of Maybe\'s:
 the server can't send requests to the client (due to the single-threaded client, see TODO);
 we don't want to waste 100 ms in multiple requests after every recognition.

parameterized over your choice of string-like type (or any type at all).

-}
-- data DGNUpdate a = DGNUpdate
--  { _dgnUpdateRules   :: Maybe a -- ^ serializes to a JSON String 'Value' or 'Null'
--  , _dgnUpdateExports :: Maybe [a] -- ^ serializes to a JSON 'Array' or 'Null'
--  , _dgnUpdateLists   :: Maybe (Map String [a]) -- ^ serializes to a JSON 'Object' or 'Null'
--  } deriving (Show,Eq,Ord,Generic)

-- instance ToJSON a => ToJSON (DGNUpdate a) where
--  toJSON = genericToJSON defaultOptions{ fieldLabelModifier = stripUnderscorePrefix }
--   where stripUnderscorePrefix = (\case ('_':xs) -> xs; xs -> xs)

