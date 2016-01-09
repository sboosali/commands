{-# LANGUAGE TemplateHaskell, DeriveAnyClass #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-| 

> -- for doctests 
>>> :set -XOverloadedStrings  
>>> import Data.Aeson

-}
module Commands.Servers.Servant.Types where
import           Commands.Extra

import           Data.Aeson (ToJSON,FromJSON) 
import           Data.Text.Lazy                        (Text)
import Control.Lens 
import           Servant.Client (ServantError) 
import         Servant (ServantErr) 

import           Control.Monad.Trans.Either            (EitherT)


{-| a response, when Haskell is the server. 

-}
type Response = EitherT ServantErr IO -- TODO V c v 


{-| a response, when Haskell is the client. 

-}
type ClientResponse = EitherT ServantError IO () 


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


{-| 

-}
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
type ReloadRequest = () -- TODO replace with ping  


{-| 

-}
type ContextRequest = () 


{-| a "pointer" to a `ResObj` kept by the natlink client. 

(actually, an identifier, currently unmanaged and non-opaque, and may or may not exist). 

-}
data ForeignResultsObject = ForeignResultsObject Integer 
 deriving (Show,Read,Eq,Ord,Data,Generic,ToJSON,FromJSON)


{-| each Nothing means it's been already read by a handler, which should have updated the client.  

>>> encode $ DNSResponse Nothing Nothing Nothing 
"{\"_responseCorrection\":null,\"_responseMicrophoneState\":null,\"_responseDNSMode\":null}"

-}
data DNSResponse = DNSResponse -- TODO Rec Maybe [] 
 { _responseCorrection      :: Maybe CorrectionResponse
 , _responseVMode         :: Maybe VMode 
 -- , _responseDNSMode         :: Maybe Mode 
 , _responseMicrophoneState :: Maybe MicrophoneState 
 , _responseContext         :: Maybe String -- or c, but then the API types themselves are parameterized 
-- , response :: Maybe 
 } 
 deriving (Show,Read,Eq,Ord,Data,Generic,ToJSON,FromJSON) -- TODO Monoid, like First  


{-| 

-}
data VMode
 = NormalMode 
 | CorrectingMode 

 | DictatingMode 
 | SleepingMode  
 | OffMode  
 | ReadingMode 

 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic,ToJSON,FromJSON)


{-| 

-}
data DNSMode 
 = CommandsMode
 | DictationMode
 | SpellingMode
 | NumberMode
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic,ToJSON,FromJSON)


{-| 

-}
data MicrophoneState 
 = MicrophoneOn 
 | MicrophoneAsleep 
 | MicrophoneOff 
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic,ToJSON,FromJSON)



-- ================================================================ --

emptyDNSResponse :: DNSResponse
emptyDNSResponse = DNSResponse Nothing Nothing Nothing Nothing 


-- ================================================================ --

makeLenses ''DNSResponse
makeLenses ''CorrectionResponse 
makePrisms ''DNSMode
makePrisms ''MicrophoneState 
