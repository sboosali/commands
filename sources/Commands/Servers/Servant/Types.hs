{-# LANGUAGE DataKinds, DeriveAnyClass #-}
{-# LANGUAGE LambdaCase, RankNTypes, RecordWildCards, TypeOperators, TemplateHaskell   #-}
{-| 

> -- for doctests 
>>> :set -XOverloadedStrings  
>>> import Data.Aeson


git  

-}
module Commands.Servers.Servant.Types where
import           Commands.Extra
import qualified Commands.Frontends.Dragon13.Serialize as DNS
import           Commands.Parsers.Earley              (EarleyParser)

import Control.Lens
import           Control.Monad.Trans.Either            (EitherT)
import           Data.Aeson (ToJSON,FromJSON) 
import           Data.Text.Lazy                        (Text)
import qualified Network.Wai.Handler.Warp              as Wai
import           Servant
import           Servant.Client (ServantError) 

import Control.Concurrent.STM


{-| a response, when Haskell is the server. 

-}
type Response = EitherT ServantErr IO

{-| a response, when Haskell is the client. 

-}
type ClientResponse = EitherT ServantError IO () 

{-| the complete API between a @command server@ and a @natlink client@. 

-}
type NatlinkAPI = RecognitionAPI :<|> HypothesesAPI :<|> CorrectionAPI :<|> ReloadAPI :<|> ContextAPI

{- | the API for a successful recognition. 

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
type RecognitionAPI = "recognition" :> ReqBody '[JSON] RecognitionRequest :> Post '[JSON] (DNSResponse) 

type HypothesesAPI = HypothesesAPIOf (DNSResponse)

type HypothesesClientAPI = HypothesesAPIOf () 

{-| the API for correcting recognition, given Dragon's hypotheses. 

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
type HypothesesAPIOf a = "hypotheses" :> ReqBody '[JSON] HypothesesRequest :> Post '[JSON] a 

{-| the API for TODO 

-}
type CorrectionAPI = "correction" :> ReqBody '[JSON] CorrectionRequest :> Post '[JSON] (DNSResponse) 

{-| the API for TODO 

-}
type ReloadAPI = "reload" :> ReqBody '[JSON] ReloadRequest :> Post '[JSON] (DNSResponse) 

{-| the API for TODO 

-}
type ContextAPI = "context" :> ReqBody '[JSON] ContextRequest :> Post '[JSON] (DNSResponse) 

{- | a successful recognition of an utterance. 

>>> 'decode' "[\"hello\",\"world\"]" :: Maybe RecognitionRequest
Just (RecognitionRequest ["hello","world"])

-}
newtype RecognitionRequest = RecognitionRequest [Text]
 deriving (Show,Read,Eq,Ord,Data,Generic,ToJSON,FromJSON)

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

{-| a correction for the given recognition. 

>>> 'encode' (CorrectionResponse (ForeignResultsObject 0, ["the", "correction", "response"]))
"[0,[\"the\",\"correction\",\"response\"]]"

-}
data CorrectionResponse = CorrectionResponse (ForeignResultsObject, [Text]) 
 deriving (Show,Read,Eq,Ord,Data,Generic,ToJSON,FromJSON)

{-| 

-}
type ReloadRequest = () 

{-| 

-}
type ContextRequest = () 

{-| a "pointer" to a `ResObj` kept by the natlink client. 

(actually, an identifier, currently unmanaged and non-opaque, and may or may not exist). 

-}
data ForeignResultsObject = ForeignResultsObject Integer 
 deriving (Show,Read,Eq,Ord,Data,Generic,ToJSON,FromJSON)

{- | read-only.

"static" configuration.

-}
data VSettings m c a = VSettings
 { vPort                 :: Wai.Port
 , vSetup                :: VSettings m c a -> IO (Either VError ())
 , vInterpretRecognition :: VHandler m c a RecognitionRequest
 , vInterpretHypotheses  :: VHandler m c a HypothesesRequest 
 , vInterpretCorrection  :: VHandler m c a CorrectionRequest 
 , vInterpretReload      :: VHandler m c a ReloadRequest     
 , vInterpretContext     :: VHandler m c a ContextRequest    
 , vConfig               :: VConfig m c a
 , vUIAddress            :: Address 
 , vGlobals              :: VGlobals c 
 -- , vUpdateConfig   :: VPlugin :~>: VConfig
 }

{-| 

-}
type VHandler m c a i = VSettings m c a -> i -> Response DNSResponse
-- newtype VHandler m c a i = VHandler { getVHandler :: VSettings m c a -> i -> Response DNSResponse }  -- contravariant 
-- TODO type VHandlers m c a is = Rec (VHandler m c a) is

{- | read-only.
"dynamic" configuration
-}
data VConfig m c a = VConfig
 { vGrammar :: DNS.SerializedGrammar
 , vParser  :: (forall s r. EarleyParser s r String Text a) 
 , vDesugar :: c -> a -> m ()
 }

{- |
-}
data VError = VError String
 deriving (Show,Read,Eq,Ord,Data,Generic)

{-| 

naming: 

* @c@ for "context". a type parameter, for extensibility. 

-}
data VGlobals c = VGlobals 
 { vResponse :: TVar DNSResponse -- ^ 
 , vMode :: TVar VMode 
 , vContext :: TVar c 
 } 
 -- TODO deriving (Generic)

{-| each Nothing means it's been already read by a handler, which should have updated the client.  

>>> encode $ DNSResponse Nothing Nothing Nothing 
"{\"_responseCorrection\":null,\"_responseMicrophoneState\":null,\"_responseDNSMode\":null}"

-}
data DNSResponse = DNSResponse -- TODO Rec Maybe [] 
 { _responseCorrection      :: Maybe CorrectionResponse
 , _responseDNSMode         :: Maybe DNSMode 
 , _responseMicrophoneState :: Maybe MicrophoneState 
 , _responseContext         :: Maybe String -- or c, but then the API types themselves are parameterized 
-- , response :: Maybe 
 } 
 deriving (Show,Read,Eq,Ord,Data,Generic,ToJSON,FromJSON) -- TODO Monoid, like First  

data VMode
 = RecognitionMode 
 | CorrectionMode 
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic,ToJSON,FromJSON)

data DNSMode 
 = CommandsMode
 | DictationMode
 | SpellingMode
 | NumberMode
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic,ToJSON,FromJSON)

data MicrophoneState 
 = MicrophoneOn 
 | MicrophoneAsleep 
 | MicrophoneOff 
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic,ToJSON,FromJSON)

natlinkAPI :: Proxy NatlinkAPI
natlinkAPI = Proxy

recognitionAPI :: Proxy RecognitionAPI
recognitionAPI = Proxy 

hypothesesAPI :: Proxy HypothesesAPI
hypothesesAPI = Proxy 

hypothesesClientAPI :: Proxy HypothesesClientAPI
hypothesesClientAPI = Proxy 

correctionAPI :: Proxy CorrectionAPI
correctionAPI = Proxy 

reloadAPI :: Proxy ReloadAPI
reloadAPI = Proxy 

contextAPI :: Proxy ContextAPI
contextAPI = Proxy 

emptyDNSResponse :: DNSResponse
emptyDNSResponse = DNSResponse Nothing Nothing Nothing Nothing 


-- ================================================================ --

makeLenses ''DNSResponse
makeLenses ''CorrectionResponse 
makePrisms ''DNSMode
makePrisms ''MicrophoneState 
