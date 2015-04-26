{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-matches #-}
module Commands.Backends.OSX.Example where
import Commands.Backends.OSX.DSL
import Commands.Backends.OSX.Execute
import Commands.Backends.OSX.Types

import Control.Monad                 (replicateM_)
import Data.Monoid                   ((<>))


main = do
 attemptActions testDerived

attemptActions a = do
 putStrLn ""
 putStrLn $ showActions a
 runActions a

testDerived = do
 _ <- copy
 delay 100
 paste

testDSL :: Actions ClipboardContents
testDSL = do

 -- delay 30
 sendKeyPress [Command, Shift] BKey
 delay 1000
 sendKeyPress [Command] DownArrowKey

 app <- currentApplication
 s <- getClipboard
 openURL $ "https://www.google.com/search?q=" <> s
 setClipboard app
 getClipboard

markWord = do
 sendKeyPress [Option       ] LeftArrowKey
 sendKeyPress [Option, Shift] RightArrowKey

backWord = do
 sendKeyPress [Option] LeftArrowKey

forWord = do
 sendKeyPress [Option] RightArrowKey


-- keyboard shortcuts don't need lag between each Keypress (hence
-- 'replicateM_', without 'interleave $ delay 25000'). only
-- interaction needs lag (e.g. a mini-buffer pop-up).
-- tested in Chrome.
testChrome :: Actions ()
testChrome = do
 delay 5000
 replicateM_ 10 forWord
 delay 1000
 replicateM_ 10 backWord
 delay 1000
 markWord

