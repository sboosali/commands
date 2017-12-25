{-# LANGUAGE  TemplateHaskell, OverloadedStrings, PostfixOperators, RankNTypes, LambdaCase, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Shortcut where
import Commands.Plugins.Spiros.Shortcut.Types
import Commands.Plugins.Spiros.Chrome.Gmail

import Commands.Extra
import Commands.Mixins.DNS13OSX9


-- TODO global context (e.g. all Apps) should be overridden by a local context (e.g. some App)
myShortcuts :: R Shortcut
myShortcuts = 'myShortcuts
 <=> globalShortcuts
 <|> emacsShortcuts
 <|> magitShortcuts
 <|> tagsShortcuts
 <|> haskellShortcuts
 <|> chromeShortcuts
 <|> gmailShortcuts


globalShortcuts :: R Shortcut
globalShortcuts = shortcuts
 -- keys
 [ "space"-: "<spc>"
 , "tab"-: "<tab>"
 , "ret"-: "<ret>"  -- "line" conflicts with (Line :: Region)
 , "yes"-: "<ret>"  -- only active (or at least active) during correction mode
 , "del"-: "<del>"
 , "return"-: "<ret>"
 , "delete"-: "<del>"
 , "up"-: "<up>"
 , "down"-: "<down>"
 , "left"-: "<left>"
 , "right"-: "<right>"
 , "F3"-: "<f3>"
 , "F4"-: "<f4>"
 , "next tab"-: "C-<tab>"-- TODO 
 , "back tab"-: "C-S-<tab>"-- TODO 
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""

 , "no"-: "H-z"
 , "undo"-: "H-z"
 , "salt"-: "H-a"
 , "pasting"-: "H-v"              -- "paste" is mis-recognized "eighth"
 , "copying"-: "H-c"               --
 , "cutting"-: "H-x"                --
 , "kill"-: "C-k"
 , "killer"-: "C-k"
-- , "show apps"-: "M-<tab>" -- <ret> only needed on OS X
 -- , "twist"-: "M-<tab> <ret>"
 , "twist"-: "M-<tab>" -- <ret> only needed on OS X
 , "switch"-: "M-`"
 -- , "abdicate"-: "M-q" --
 , "abdicate"-: "A-<f4>"
 , "scroll"-: "<spc>"
 , "scroll down"-: "<spc>"
 , "scroll up"-: "S-<spc>"
 , "submit"-: "<tab> <ret>"                 -- "M-<ret>"
 , "preferences"-: "M-,"
 , "save"-: "H-s"
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 ]


emacsShortcuts :: R Shortcut
emacsShortcuts = shortcuts
 [ "stop"-: "C-g"
 -- , "check"-: "M-u"
 -- , "comment"-: "M-;"
 , "yank"-: "C-y"              --
 , "undo"-: "C-/"
 , "redo"-: "C-?"               -- NOTE undo-tree-mode
 , "mark"-: "C-<spc>"
 , "new file"-: "C-\\"
 , "replace"-: "M-r"
 , "close buffer"-: "C-x k"
 , "buffers"-: "C-x b"
 , "close"-: "C-c C-c"
 , "full-screen"-: "C-x 1"
 , "split screen"-: "C-x 2"
 , "other buffer"-: "C-x b <ret>"
 , "other window"-: "C-x o"
 , "evil toggle"-: "C-z"
-- , "divider"-: "M-<dash>"                -- TODO (M-- doesn't parse)
 -- , "yank"-: "C-y"               -- works in many buffer, M-v doesn't . TODO this is how we want to paste and Emacs, including phrases
 , "record"-: "<f3>"
 , "repeat"-: "<f4>"
 , "read-only"-: "C-x C-q"
 , "read/write"-: "C-x C-q"
 , ""-: ""
 , ""-: ""
 , ""-: ""
 ]

tagsShortcuts = shortcuts             -- TAGS
 [ ""-: ""
 , "definition"-: "M-."
 , "jump def"-: "M-."
 , "jump back"-: "M-*"
 , ""-: ""
 , ""-: ""
 , ""-: ""
 ]

magitShortcuts :: R Shortcut
magitShortcuts = shortcuts
 [  ""-: ""
 -- , "stage"-: "s"
 -- , "stage all"-: "S-s"
 -- , "unstage"-: "u"
 -- , "unstage all"-: "S-u y"
 -- , "commit"-: "c c"
 -- , "amend"-: "c a"
 -- , "difference"-: "d <ret>"
 -- , "push"-: "S-p S-p"
 -- , "chunks"-: "<tab>"           -- nonstandard
 -- , ""-: ""
 ]

haskellShortcuts :: R Shortcut
haskellShortcuts = shortcuts
 [ ""-: ""
 , "type"-: "C-c C-t"
 , "insert type"-: "C-u C-c C-t"
 , "info"-: "C-c C-i"                             -- haskell-process-do-info
 , "builder"-: "C-c C-c"
-- , "restart"-: "M-x <ret> haskell-process-restart" -- TODO won't parse naked chars
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 ]

chromeShortcuts :: R Shortcut
chromeShortcuts = shortcuts
 [ "new tab"-: "6 H-t"
 , "close tab"-: "H-w"
 , "last"-: "H-9"
 , "left tab"-: "C-S-<tab>"
 , "right tab"-: "C-<tab>"
 , "left page"-: "M-<left>"
 , "right page"-: "M-<right>"
 , "reload"-: "H-r"
 , "zoom in"-: "H-+"
 , "zoom out"-: "H-<dash>"                -- TODO (M-- doesn't parse)
 , "reopen tab"-: "H-S-t"
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 ]
