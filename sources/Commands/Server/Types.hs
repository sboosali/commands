module Commands.Server.Types where

-- import Commands.Frontends.Natlink.Types
import Commands.Frontends.Dictation
import Digit

import Prelude.Spiros
import Prelude()

{-| a user can choose:

* one of the (ten or fewer) hypotheses they are presented with.
* an arbitrary sentence (possibly an edited hypothesis).

(The speech engine might then reject it sounds too different
from the recognition being corrected).

-}
data Correction
 = ChosenCorrection Digit
 | SpokenCorrection Dictation
 | EditedCorrection Digit
