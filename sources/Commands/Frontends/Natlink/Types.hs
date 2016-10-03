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
data NatlinkF k
 = NatlinkF

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

--------------------------------------------------------------------------------

makePrisms ''Recognition
makeLenses ''DNSWord
makePrisms ''DNSMode
makePrisms ''MicrophoneState
