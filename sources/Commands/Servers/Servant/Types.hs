{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DataKinds, DeriveAnyClass, DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE LambdaCase, RankNTypes, RecordWildCards, TypeOperators  #-}
{-| 

> -- for doctests 
>>> :set -XOverloadedStrings  
>>> import Data.Aeson


-}
module Commands.Servers.Servant.Types where
import qualified Commands.Backends.OSX                 as OSX
import           Commands.Extra
import qualified Commands.Frontends.Dragon13.Serialize as DNS
import           Commands.Mixins.DNS13OSX9             (EarleyParser, RULED)

import           Control.Monad.Trans.Either            (EitherT)
import           Data.Aeson (ToJSON,FromJSON) 
import           Data.Text.Lazy                        (Text)
import qualified Network.Wai.Handler.Warp              as Wai
import           Servant


type Response = EitherT ServantErr IO

{-| the complete API between a @command server@ and a @natlink client@. 

-}
type NatlinkAPI = RecognitionAPI :<|> HypothesesAPI

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
type RecognitionAPI = "recognition" :> ReqBody '[JSON] DGNRecognition :> Post '[JSON] ()
-- type RecognitionAPI = "recognition" :> ReqBody DGNRecognition :> Post (DGNUpdate String)

{-| 

-}
type HypothesesAPI = "hypotheses" :> ReqBody '[JSON] HypothesesRequest :> Post '[JSON] () 

{- | 

>>> 'decode' "[\"hello\",\"world\"]" :: Maybe DGNRecognition
Just (DGNRecognition ["hello","world"])

-}
newtype DGNRecognition = DGNRecognition [Text]
 deriving (Show,Read,Eq,Ord,Data,Generic,FromJSON)

{-| 

>>> 'encode' (HypothesesRequest [["hello", "world"], ["below", "furled"]])
"[[\"hello\",\"world\"],[\"below\",\"furled\"]]"
>>> 'decode' "[[\"hello\",\"world\"],[\"below\",\"furled\"]]" :: Maybe HypothesesRequest
Just (HypothesesRequest [["hello","world"],["below","furled"]])


-}
newtype HypothesesRequest = HypothesesRequest [Hypothesis]
 deriving (Show,Read,Eq,Ord,Data,Generic,ToJSON,FromJSON)
-- Object (fromList [("theHypotheses",Array (fromList [Array (fromList [String "hello",String "world"]),Array (fromList [String "below",String "furled"])]))])

type Hypothesis = [Text] 

{- | read-only.

"static" configuration.

-}
data VSettings m z a = VSettings
 { vPort                 :: Wai.Port
 , vSetup                :: VSettings m z a -> IO (Either VError ())
 , vInterpretRecognition :: (forall r. RULED (VSettings m) r a) -> [Text] -> Response ()
 , vInterpretHypotheses  :: (forall r. RULED (VSettings m) r a) -> [[Text]] -> Response () 
 , vConfig               :: VConfig m z a
 -- , vUpdateConfig   :: VPlugin z :~>: VConfig z
 }

{- | read-only.
"dynamic" configuration
-}
data VConfig m z a = VConfig
 { vGrammar :: DNS.SerializedGrammar
 , vParser  :: EarleyParser z a
 , vDesugar :: OSX.Application -> a -> m ()
 }

{- |
-}
data VError = VError String
 deriving (Show,Read,Eq,Ord,Data,Generic)

