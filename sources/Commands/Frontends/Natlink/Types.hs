{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, TemplateHaskell #-}

{-| the speech engine API, as exposed by
<https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L106 Natlink>

NOTE natlink is single-threaded, and so it can only act as a client, not a server

TODO websockets + setTimerCallback ?

-}
module Commands.Frontends.Natlink.Types where
import Commands.Frontends.Dragon13.Extra

import Control.Lens(makeLenses,makePrisms)
import           Data.Aeson (ToJSON,FromJSON) --TODO rm

--import Prelude.Spiros
--import Prelude()

{-|
-}

{-|
-}
newtype Recognition = Recognition [DNSWord]
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable,Semigroup,Monoid)

{-| the "leaves" of the grammar.

given @DNSPronounced written spoken@, the speech recognition engine
recognizes @spoken@ and transcribes it as @written@.

e.g.

@
DNSWord
 { dnsPronounced="a"
 , dnsWritten="A"
 , dnsCategory="letter"
 }
@

-}
data DNSWord = DNSWord
 { dnsPronounced :: String
 , dnsWritten    :: String
 , dnsCategory   :: String
 }
 deriving (Show,Read,Eq,Ord,Data,Generic)
instance NFData   DNSWord
instance Hashable DNSWord

{-| <http://www.nuance.com/naturallyspeaking/customer-portal/documentation/userguide/chapter7/ug_chapter7_switch_recognition_mode.asp>

-}
data DNSMode
 = NormalMode
 | DictationMode
 | CommandMode
 | NumbersMode
 | SpellMode
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic)
instance NFData   DNSMode
instance Hashable DNSMode
instance ToJSON   DNSMode
instance FromJSON DNSMode

{-|

-}
data MicrophoneState
 = MicrophoneOn
 | MicrophoneAsleep
 | MicrophoneOff
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic)
instance NFData   MicrophoneState
instance Hashable MicrophoneState
instance ToJSON   MicrophoneState
instance FromJSON MicrophoneState

data GrammarObject = GrammarObject Int

data ResultsObject = ResultsObject Int

{-|
-}
data NatlinkF k
 = RecognitionMimic  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L209)
 | ExecuteScript  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L190)
 | InputFromFile  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L249)

 | SetMicrophoneState  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L178)
 | GetMicrophoneState  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L182)

 | SetTimerCallback  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L279)

 | SetWordInfo  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L466)
 | GetWordInfo  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L373)
 | DeleteWord  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L421)
 | AddWord  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L428)

 | LoadGrammarObject  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L574)
 | ActivateGrammarRule  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L601)
 | DectivateGrammarRule  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L614)
 | SetExclusiveGrammar  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L619)

 | GetResultsObject  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L759)
 | GetResultsObjectAudio  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L753)
 | CorrectResultsObject  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L740)

class MonadNatlink m where

 executeScript :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L190)
 recognitionMimic :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L209)

 setMicrophoneState :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L178)
 getMicrophoneState :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L182)

 setTimerCallback :: {-Python-}String -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L279)

 setWordInfo :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L466)
 getWordInfo :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L373)
 deleteWord :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L421)
 addWord :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L428)

 loadGrammarObject :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L574)
 activateGrammarRule :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L601)
 dectivateGrammarRule :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L614)
 setExclusiveGrammar :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L619)

 getResultsObject :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L759)
 getResultsObjectAudio :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L753)
 correctResultsObject :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L740)

--------------------------------------------------------------------------------

makePrisms ''Recognition
makeLenses ''DNSWord
makePrisms ''DNSMode
makePrisms ''MicrophoneState
