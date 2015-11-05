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
import           Servant.Client (ServantError) 


{-| a response, when Haskell is the server. 

-}
type Response = EitherT ServantErr IO

{-| a response, when Haskell is the client. 

-}
type ClientResponse = EitherT ServantError IO () 

{-| the complete API between a @command server@ and a @natlink client@. 

-}
type NatlinkAPI = RecognitionAPI :<|> HypothesesAPI :<|> CorrectionAPI

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
type RecognitionAPI = "recognition" :> ReqBody '[JSON] RecognitionRequest :> Post '[JSON] ()
-- type RecognitionAPI = "recognition" :> ReqBody RecognitionRequest :> Post (DGNUpdate String)

{-| 

* derive a Haskell __client__ and a UI __server__. 
* derive a Haskell __server__ (that forwards to the UI server) 

e.g. UI server: Emacs can embed a server and has a good UI 

e.g. trace:

@
frontend  --hypothesis-->  commands  --hypothesis(forwarded)-->  ui  --correction-->  commands  
... 
frontend  --(anything)-->  commands  --update(+correction)-->  frontend 
@

the @...@ means that we must wait until the next frontend request, 
as we assume the front-end can't embed a server. e.g. the Dragon NaturallySpeaking front-end (Natlink) 
is single-threaded and callback-driven. thus, most of our http responses are @()@, 
and we pack their responses into a single @Update@.  


-}
type HypothesesAPI = "hypotheses" :> ReqBody '[JSON] HypothesesRequest :> Post '[JSON] () 

{-| 

-}
type CorrectionAPI = "correction" :> ReqBody '[JSON] CorrectionRequest :> Post '[JSON] () 

{- | 

>>> 'decode' "[\"hello\",\"world\"]" :: Maybe RecognitionRequest
Just (RecognitionRequest ["hello","world"])

-}
newtype RecognitionRequest = RecognitionRequest [Text]
 deriving (Show,Read,Eq,Ord,Data,Generic,FromJSON)

{-| the hypotheses of the previous recognition. 

its length is between one and ten. 

>>> 'encode' (HypothesesRequest [["hello", "world"], ["below", "furled"]])
"[[\"hello\",\"world\"],[\"below\",\"furled\"]]"
>>> 'decode' "[[\"hello\",\"world\"],[\"below\",\"furled\"]]" :: Maybe HypothesesRequest
Just (HypothesesRequest [["hello","world"],["below","furled"]])


-}
newtype HypothesesRequest = HypothesesRequest [Hypothesis] -- TODO vinyl-vector: Vector 10 Maybe Hypothesis
 deriving (Show,Read,Eq,Ord,Data,Generic,ToJSON,FromJSON)
-- Object (fromList [("theHypotheses",Array (fromList [Array (fromList [String "hello",String "world"]),Array (fromList [String "below",String "furled"])]))])

type Hypothesis = [Text] 

{-| 

-}
data CorrectionRequest = CorrectionRequest [Text]
 -- { theCorrection :: Either Digit Dictation 
 -- }
 deriving (Show,Read,Eq,Ord,Data,Generic,ToJSON,FromJSON)

{- | read-only.

"static" configuration.

-}
data VSettings m z a = VSettings
 { vPort                 :: Wai.Port
 , vSetup                :: VSettings m z a -> IO (Either VError ())
 , vInterpretRecognition :: (forall r. RULED (VSettings m) r a) -> RecognitionRequest -> Response ()
 , vInterpretHypotheses  :: (forall r. RULED (VSettings m) r a) -> HypothesesRequest  -> Response () 
 , vInterpretCorrection  :: (forall r. RULED (VSettings m) r a) -> CorrectionRequest  -> Response ()
 , vConfig               :: VConfig m z a
 , vUIAddress            :: Address 
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

natlinkAPI :: Proxy NatlinkAPI
natlinkAPI = Proxy

recognitionAPI :: Proxy RecognitionAPI
recognitionAPI = Proxy 

hypothesesAPI :: Proxy HypothesesAPI
hypothesesAPI = Proxy 

correctionAPI :: Proxy CorrectionAPI
correctionAPI = Proxy 

