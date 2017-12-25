{-# LANGUAGE LambdaCase, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Plugins.Spiros.Act.Execute where 
import           Commands.Plugins.Spiros.Extra
import Commands.Plugins.Spiros.Types 
import           Commands.Plugins.Spiros.Act.Types 
import           Commands.Plugins.Spiros.Edit.Run
import           Commands.Plugins.Spiros.Emacs

import           Commands.Backends.Workflow (sendKeySequence)

import Control.Lens

--------------------------------------------------------------------------------

runActs context = \case
 ActsRW n a -> runRepeat emacsDelay n (runAct context a)

runAct context = \case
 KeyRiff_ kr -> sendKeySequence kr
 --TODO Click_ _c   -> nothing
 Edit_ a     -> whenJust (context ^? _EmacsContext) $ editEmacs a
 Move_ a     -> whenJust (context ^? _EmacsContext) $ moveEmacs a

--------------------------------------------------------------------------------

rankActs = \case
 ActsRW _i a -> rankAct a

rankAct = \case
 KeyRiff_ _kr -> highRank
 -- TODOClick_ _c    -> defaultRank
 Edit_ e      -> defaultRank + rankEdit e
 Move_ m     -> defaultRank + rankMove m

--------------------------------------------------------------------------------
