{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}

{-|
-}
module Commands.Server.Correction.Types where

-- import Commands.Frontends.Natlink.Types
import Commands.Frontends.Dictation
import Digit

import Prelude.Spiros
import Prelude()
{-|
-}

{-|
-}
data CorrectionSettings m = CorrectionSettings
 { _promptCorrection  :: Hypotheses -> m Dictation
 , _reachCorrectionUi :: m ()
 , _leaveCorrectionUi :: m ()
 }

--------------------------------------------------------------------------------

{-| a user can choose:

* one of the (ten or fewer) hypotheses they are presented with.
* an arbitrary sentence (possibly an edited hypothesis).

(The speech engine might then reject it sounds too different
from the recognition being corrected).

-}
data Correction
 = ChosenCorrection Digit
 | SpokenCorrection Dictation
 | EditedCorrection Digit --TODO to preserve DNSTokens metadata

 {-| the hypotheses of the previous recognition.
 its length is between one and ten.

 -}
newtype Hypotheses = Hypotheses [Hypothesis]
 deriving (Show,Read,Eq,Ord,Data,Generic,Hashable,NFData)

{-|
-}
type Hypothesis = [Text]
