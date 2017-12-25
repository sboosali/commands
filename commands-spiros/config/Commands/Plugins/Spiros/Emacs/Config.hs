{-# LANGUAGE ExtendedDefaultRules, FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
module Commands.Plugins.Spiros.Emacs.Config where

import Control.Monad
import qualified Commands.Backends.Workflow as W

-- default ((), Actions_)


emacsDelay = 10 :: Int                 -- milliseconds

-- TODO read application from environment, which determines the keyboard shortcut
-- an application is defined by the keyboard shortcuts it supports?
-- Rec?
-- Map String Workflow
-- lookup "mark"
mark = W.press "C-<spc>"

activate_mark = replicateM_ 2 exchange_point_and_mark

exchange_point_and_mark = W.press "C-x C-x"
-- exchange_point_and_mark = runEmacs "exchange-point-and-mark"

execute_extended_command = W.press "C-w" --TODO non-standard: make this configurable? ImplicitParams? this is the configuration! just put in separate module. or define this as a keypress, and explicitly turn it into an action at  use site.

eval_expression = W.press "M-:"

window_bottom = W.press "M-<down>"

-- my_keymap_prefix = (kbd"M-q")
-- keymap = "M-q"                  -- NOTE nonstandard  
keymap = "C-x C-y"

haskell_interactive_bring = W.press "C-`"      -- haskell-mode 

haskell_compile = W.press "C-c C-c"      -- haskell-mode 

