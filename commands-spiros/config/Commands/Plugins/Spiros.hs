{-| (re-exports)

re-export everything, for convenient importing, and for `ghci`:

@
:m +Commands.Plugins.Spiros
@

-}
module Commands.Plugins.Spiros
 ( module Commands.Plugins.Spiros.Extra
 , module Commands.Plugins.Spiros.Types
 , module Commands.Plugins.Spiros.TypeLevel
 , module Commands.Plugins.Spiros.Main
 , module Commands.Plugins.Spiros.Apply
--, module Commands.Plugins.Spiros.Finite
 , module Commands.Plugins.Spiros.Digit.Grammar
 -- , module Commands.Plugins.Spiros.Correct
 -- , module Commands.Plugins.Spiros.Server
 -- -- , module Commands.Plugins.Spiros.Server.Workflow
 -- , module Commands.Plugins.Spiros.Shim
 , module Commands.Plugins.Spiros.Template
 -- grammars
 , module Commands.Plugins.Spiros.Root
 , module Commands.Plugins.Spiros.Macros
 , module Commands.Plugins.Spiros.Shortcut
 , module Commands.Plugins.Spiros.Shell
 , module Commands.Plugins.Spiros.Keys
 , module Commands.Plugins.Spiros.Number
 , module Commands.Plugins.Spiros.Phrase
 , module Commands.Plugins.Spiros.Edit
 , module Commands.Plugins.Spiros.Emacs
 , module Commands.Plugins.Spiros.Chrome
 ) where

import Commands.Plugins.Spiros.Extra
import Commands.Plugins.Spiros.Types
import Commands.Plugins.Spiros.TypeLevel
import Commands.Plugins.Spiros.Main
import Commands.Plugins.Spiros.Apply
-- import Commands.Plugins.Spiros.Finite
import Commands.Plugins.Spiros.Digit.Grammar
-- import Commands.Plugins.Spiros.Correct
-- import Commands.Plugins.Spiros.Server
-- import Commands.Plugins.Spiros.Server.Workflow
-- import Commands.Plugins.Spiros.Shim
import Commands.Plugins.Spiros.Template
import Commands.Plugins.Spiros.Root
import Commands.Plugins.Spiros.Macros
import Commands.Plugins.Spiros.Shortcut
import Commands.Plugins.Spiros.Shell
import Commands.Plugins.Spiros.Keys
import Commands.Plugins.Spiros.Number
import Commands.Plugins.Spiros.Phrase
import Commands.Plugins.Spiros.Edit
import Commands.Plugins.Spiros.Emacs
import Commands.Plugins.Spiros.Chrome
