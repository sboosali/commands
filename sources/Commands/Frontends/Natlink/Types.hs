{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, PatternSynonyms, OverloadedStrings #-}

{-| the speech engine API, as exposed by
<https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L106 Natlink>

NOTE natlink is single-threaded, and so it can only act as a client, not a server

TODO websockets + setTimerCallback ?

-}
module Commands.Frontends.Natlink.Types where
import Commands.Frontends.Dragon13.Extra
import Commands.Frontends.Dragon13.Types (DNSGrammar(..), DNSInfo)
import Commands.Frontends.Dragon13.Text

import           Data.Aeson (ToJSON,FromJSON) --TODO rm
import Data.Maybe (isJust)
import           Data.Text.Lazy (Text)
--import qualified Data.Text.Lazy as T

import Prelude()

--------------------------------------------------------------------------------

{-|

-}
newtype DNSBuffer = DNSBuffer String

{-|

-}
newtype Recognition = Recognition [DNSToken]
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable,ToJSON,FromJSON,Semigroup,Monoid)

{-| the "leaves" of the grammar.

Returned by Dragon NaturallySpeaking when an utterance is recognized.

given @DNSPronounced written spoken@, the speech recognition engine
recognizes @spoken@ and transcribes it as @written@.

e.g.

@
DNSToken
 { dnsPronounced="a"
 , dnsWritten="A"
 , dnsCategory="letter"
 }
@

-}
data DNSToken = DNSToken
 { _dnsPronounced :: Text
 , _dnsWritten    :: Text
 , _dnsCategory   :: Text
 }
 deriving (Show,Read,Eq,Ord,Data,Generic)
instance NFData   DNSToken
instance Hashable DNSToken
instance ToJSON   DNSToken
instance FromJSON DNSToken

toDNSToken :: Text -> Maybe DNSToken --TODO
toDNSToken s = if s & (validDNSToken > isJust) then Just DNSToken{..} else Nothing
 where
 _dnsPronounced = s
 _dnsWritten = ""
 _dnsCategory = ""

validDNSToken :: Text -> Maybe Text --TODO
validDNSToken s = Just s

{-| the "leaves" of the grammar.

Provided to Dragon NaturallySpeaking in the grammar that is loaded.

Validated by 'validateDNSWord'

-}
data DNSWord = DNSWord Text

validateDNSWord :: Text -> Maybe DNSWord
validateDNSWord s = s & (DNSWord > Just) --TODO

--------------------------------------------------------------------------------

{-|
-}
pattern MAXIMUM_ALTERNATIVE_RECOGNITIONS :: Natural
pattern MAXIMUM_ALTERNATIVE_RECOGNITIONS = 10

{-| <http://www.nuance.com/naturallyspeaking/customer-portal/documentation/userguide/chapter7/ug_chapter7_switch_recognition_mode.asp>

-}
data DNSMode
 = NormalMode -- ^ Both Dictation and Commands
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

newtype PythonExpression = PythonExpression { getPythonExpression :: String }

newtype DragonScriptExpression = DragonScriptExpression { getDragonScriptExpression :: String }

{-|

Audio data in wave format.

11.025 * 4000 = 441000


-}
data Utterance = Utterance
  { _utteranceData :: () --TODO
  }

{-|

Training can fail of the transcription is not close enough to the utterance.

-}
data CorrectionStatus = CorrectionSuccess | CorrectionHeterophonic | CorrectionInvalidWord
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic)
instance NFData   CorrectionStatus
instance Hashable CorrectionStatus
instance ToJSON   CorrectionStatus
instance FromJSON CorrectionStatus

-- | True is "good", False is "bad"
booleanCorrectionStatus :: CorrectionStatus -> Bool
booleanCorrectionStatus = \case
  CorrectionSuccess      -> True
  CorrectionHeterophonic -> False
  CorrectionInvalidWord  -> False

{-|

        Can raise InvalidWord if the grammar contains an invalid word.
        Can raise BadGrammar if the grammar has syntax errors, or if it's is too complex

-}
data GrammarLoadStatus = GrammarLoadSuccess | GrammarLoadBadGrammar | GrammarLoadInvalidWord

-- | True is "good", False is "bad"
booleanGrammarLoadStatus :: GrammarLoadStatus -> Bool
booleanGrammarLoadStatus = \case
  GrammarLoadSuccess      -> True
  GrammarLoadBadGrammar   -> False
  GrammarLoadInvalidWord  -> False

--------------------------------------------------------------------------------

type ForeignIdentifier = Int

data GrammarObject = GrammarObject_ ForeignIdentifier -- { getGrammarObject :: Int }
 deriving (Show,Eq,Ord,Generic) -- Show only for debugging
instance NFData   GrammarObject
instance Hashable GrammarObject

unsafeGrammarObject :: ForeignIdentifier -> GrammarObject
unsafeGrammarObject = GrammarObject_

newtype SelectionGrammarObject = SelectionGrammarObject_ ForeignIdentifier
 deriving (Show,Eq,Ord,Generic) -- Show only for debugging
instance NFData   SelectionGrammarObject
instance Hashable SelectionGrammarObject

unsafeSelectionGrammarObject :: ForeignIdentifier -> SelectionGrammarObject
unsafeSelectionGrammarObject = SelectionGrammarObject_

data ResultsObject = ResultsObject_ ForeignIdentifier -- { getResultsObject :: Int }
   deriving (Show,Eq,Ord,Generic) -- Show only for debugging
instance NFData   ResultsObject
instance Hashable ResultsObject

unsafeResultsObject :: ForeignIdentifier -> ResultsObject
unsafeResultsObject = ResultsObject_

data SelectionResultsObject = SelectionResultsObject_ ForeignIdentifier -- { getResultsObject :: Int }
unsafeSelectionResultsObject :: ForeignIdentifier -> SelectionResultsObject
unsafeSelectionResultsObject = SelectionResultsObject_

--------------------------------------------------------------------------------

{-| Initialization/configuration properties shared by all grammars.

-}
data GrammarProperties = GrammarProperties
  { _grammarStatus            :: Status
  , _grammarExclusivity       :: Exclusivity
  , _grammarShouldEavesdrop   :: ShouldEavesdrop
  , _grammarShouldHypothesize :: ShouldHypothesize
--  , grammar ::
  }  deriving (Show,Read,Eq,Ord,Data,Generic)

{-|

-}
defaultGrammarProperties :: GrammarProperties
defaultGrammarProperties = primaryGrammarProperties

primaryGrammarProperties :: GrammarProperties
primaryGrammarProperties = GrammarProperties{..}
 where
 _grammarStatus = Enabled
 _grammarExclusivity = Inclusive
 _grammarShouldEavesdrop = YesEavesdrop
 _grammarShouldHypothesize = YesHypothesize

{-|

-}
narcissisticGrammarProperties :: GrammarProperties
narcissisticGrammarProperties = GrammarProperties{..}
 where
 _grammarStatus = Enabled
 _grammarExclusivity = Exclusive
 _grammarShouldEavesdrop = YesEavesdrop
 _grammarShouldHypothesize = YesHypothesize
 -- grammar =

{-|

-}
fastGrammarProperties :: GrammarProperties
fastGrammarProperties = GrammarProperties{..}
 where
 _grammarStatus = Enabled
 _grammarExclusivity = Inclusive
 _grammarShouldEavesdrop = NoEavesdrop
 _grammarShouldHypothesize = NoHypothesize
 -- grammar =

-- | 'Bool'-like. (A grammar can be loaded but disabled.)
data Status = Enabled | Disabled
 deriving (Show,Read,Eq,Ord,Bounded,Enum,Data,Generic) -- TODO Monoid All?

-- | 'Bool'-like. Whether the grammar is exclusive, i.e. it disables all other grammars.
data Exclusivity = Inclusive | Exclusive -- TODO Monoid Any?
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic)

-- | 'Bool'-like. Should the grammar listen to other grammars' recognitions' callbacks.
data ShouldEavesdrop = YesEavesdrop | NoEavesdrop
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic)

-- | 'Bool'-like. Should the grammar listen to each hypothesis's callback.
data ShouldHypothesize = YesHypothesize | NoHypothesize
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic)

--------------------------------------------------------------------------------

{-|

-}
data ControlGrammar = ControlGrammar
  { _controlProperties    :: GrammarProperties
  , _controlConfiguration :: DNSGrammar DNSInfo DNSText DNSName
  } deriving (Show,Eq,Ord,Generic)

-- data ControlConfiguration = ControlConfiguration
--   { _control ::
--   }

{-|

-}
data ControlResults = ControlResults
  { _controlRecognition :: Recognition
--  , _control ::
  } deriving (Show,Read,Eq,Ord,Data,Generic)


--------------------------------------------------------------------------------

{-|

Whenever the selection buffer is updated, Dragon NaturallySpeaking
implicitly constructs a grammar with this production:

@
<'selectionSelectWords'> <subsequence> [ <'selectionThroughWords'> <subsequence> ]
@

where @<selectionSelectWords>@ and @<selectionThroughWords>@ are lists, and
@<subsequence>@ is any subsequence in the selection buffer.

Manually constructing @<subsequence>@, in a (non-selection) 'Grammar', would be
extremely inefficient, as it grows quadradically in the size of the buffer.

also see @MacroSystem/core/natlinkutils.py#L755@

https://github.com/sboosali/NatLink/blob/9545436181f23652224041afa2035f12fa60d949/MacroSystem/core/natlinkutils.py#L755

"A select XYZ grammar is a special grammar which
recognizes an utterance of the form "<verb> <text> [ through <text> ]"
where <verb> is specified and <text> is an arbitrary sequence of words in
a specified text buffer."

-}
data SelectionGrammar = SelectionGrammar
  -- { selectionStatus :: Status
  -- , selectionExclusivity :: Exclusivity
  -- , selectionShouldEavesdrop :: ShouldEavesdrop
  -- , selectionShouldHypothesize :: ShouldHypothesize
  { _selectionProperties    :: GrammarProperties
  , _selectionConfiguration :: SelectionConfiguration
--  , selection ::
  } deriving (Show,Read,Eq,Ord,Data,Generic)

{-|

-}
defaultSelectionGrammar :: SelectionGrammar
defaultSelectionGrammar = SelectionGrammar{..}
 where
 _selectionProperties = defaultGrammarProperties
 _selectionConfiguration = defaultSelectionConfiguration

{-|

-}
data SelectionConfiguration = SelectionConfiguration
  { _selectionSelectWords  :: [String]
  , _selectionThroughWords :: [String]
--  , selection ::
  } deriving (Show,Read,Eq,Ord,Data,Generic)

defaultSelectionConfiguration :: SelectionConfiguration
defaultSelectionConfiguration = SelectionConfiguration{..}
 where
 _selectionSelectWords = ["select","correct","insert before","insert after","capitalize"] -- TODO Reproduce all NaturallySpeaking's Selection commands
 _selectionThroughWords = ["through","until"] -- TODO non-words, so they dont conflict with the buffer itself

data SelectionSettingStatus = SelectionSettingSuccess

{-|

see @ResultsObject.getSelectInfo()@

-}
data SelectionResults = SelectionResults
 { _sroRecognition :: Recognition -- ^ A subsequence of the selection buffer
 , _sroOffset      :: Natural     -- ^ Bounded by the length of the selection buffer -- TODO Necessary?
 } deriving (Show,Read,Eq,Ord,Data,Generic)


--------------------------------------------------------------------------------

class MonadNatlink m where

 executeScript :: DragonScriptExpression -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L190)
 recognitionMimic :: Recognition -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L209)

 setMicrophoneState :: MicrophoneState -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L178)
 getMicrophoneState :: m (MicrophoneState)  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L182)

 setTimerCallback :: DragonScriptExpression -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L279)

 loadGrammarObject :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L574)
 activateGrammarRule :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L601)
 dectivateGrammarRule :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L614)
 setExclusiveGrammar :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L619)

-- getResultsObject :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L759)
 getResultsObjectAudio :: (ResultsObject) -> m (Utterance)  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L753)
 correctResultsObject :: (ResultsObject) -> Recognition -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L740)

 setWordInfo :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L466)
 getWordInfo :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L373)
 deleteWord :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L421)
 addWord :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L428)

 setSelection :: SelectionGrammarObject -> DNSBuffer -> m SelectionSettingStatus

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

--------------------------------------------------------------------------------
