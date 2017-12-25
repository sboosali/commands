#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-unused-do-bind -fno-warn-type-defaults -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings, DoAndIfThenElse, LambdaCase #-} 
import Turtle hiding (cp,sed)
import qualified Turtle
-- import           GHC.Exts                        (fromString)
-- import qualified Data.Text.Lazy                  as T

-------------------------------------------------------------------------------
-- config

__file__ = "update.hs"
idir = "/Users/sboosalis/commands-core/"

-- there are three filepaths per (A,B) pair: the A "there" ($idir/sources/), the A "here" (./sources/), and B (./config/)
files =
 ["sources/Commands/Plugins/Example.hs"-: "config/Commands/Plugins/Spiros.hs"

 ,"sources/Commands/Plugins/Example/Root.hs"-: "config/Commands/Plugins/Spiros/Root.hs"
 ,"sources/Commands/Plugins/Example/Phrase.hs"-:"config/Commands/Plugins/Spiros/Phrase.hs"
 ,"sources/Commands/Plugins/Example/Shortcut.hs"-: "config/Commands/Plugins/Spiros/Shortcut.hs"

 ,"sources/Commands/Frontends/Dragon13/Shim.hs"-: "config/Commands/Plugins/Spiros/Shim.hs"
-- ,""-: ""
 ]

emacs_interactive_commands_file = "emacs-commands.txt" 

-------------------------------------------------------------------------------

main = do
 chmod rwx __file__

 -- the user may have forgotten to configure their own directory
 idirExists <- liftIO$ testdir idir
 unless idirExists $ do
  die$ format ("idir ("%fp%") doesn't exist") idir

 -- parse cmdln opts and run
 opts <- options "Update Script" (arg pOptions "action" "")
 runOptions opts >>= exit

data Options = PullFiles | PushFiles | MungeInteractiveCommands
 deriving (Show,Enum)

pOptions = \case
  "pull" -> Just PullFiles
  "push" -> Just PushFiles
  "munge" -> Just MungeInteractiveCommands
  _ -> Nothing

runOptions = \case
 PullFiles -> pullFiles
 PushFiles -> pushFiles
 MungeInteractiveCommands -> mungeInteractiveCommands

-------------------------------------------------------------------------------

pullFiles = do

 sh$ do
  (file,_) <- select files
  mktree (directory file)
  cp (idir<>file) file

 sh$ do
  (source,config) <- select files
  mktree (directory config)
  cp source config

 echo ""
 echo "[find sources]"
 view$ find chars "sources"

 odir <- pwd
 let filesAbs = map (\(fileA,fileB) -> (odir<>fileA, odir<>fileB)) files
 testEdiffFiles filesAbs
 echo ""
 echo "M-x eval-last-sexp"
 echoEdiffFiles filesAbs

 return ExitSuccess

testEdiffFiles
 = traverse testfileDie
 . concatMap (\(fileA,fileB) -> [fileA,fileB])

testfileDie file = do
 exists <- testfile file
 if   exists
 then return()
 else die$ format ("[testfile] file doesn't exist ("%fp%")") file

echoEdiffFiles
 = traverse echo
 . map (uncurry ediff_files)
 . bimapPair fp2txt fp2txt

-- (ediff-files FILE-A FILE-B &optional STARTUP-HOOKS)
ediff_files fileA fileB = format ("(ediff-files "%s%" "%s%")") (repr fileA) (repr fileB)
 -- show the strings for double quotes
 -- escaped double quotes: format ("(ediff-files \""%s%"\" \""%s%"\")") (fileA) (fileB)

-------------------------------------------------------------------------------

pushFiles = do

 -- copy to fake "stage"
 sh$ do
  (source,config) <- select files
  mktree (directory source)
  cp config source

 -- don't backup, expect version control
 exitCode <- withPWD idir $do
  git ["--no-pager", "diff", "--exit-code"] .&&. git ["--no-pager", "diff", "--exit-code", "--cached"]
  --git status --porcelain --untracked-files=no
 unless (exitCode==ExitSuccess) $ do
  die$ "[git diff] nonempty"

 -- fix imports, rename vars
 let pat = (("Spiros" *> return "Example") <|> ("spiros" *> return "example")) -- no 'has' pattern
 sh$ do
  (file,_) <- select files
  output (idir<>file) (sed pat (input file))

 -- print commit messages, we pseudo-"merge" between two different repos
 git ["--no-pager", "log", "--pretty=%H%n%n%B", "--all"]
 echo "M-x magit-status"
 echo "(goto filename) press d"
 echo "(goto filediff) press e"

 return ExitSuccess


-------------------------------------------------------------------------------

mungeInteractiveCommands = do
 sh$ do
  _command <- input emacs_interactive_commands_file
  die"mungeInteractiveCommands"
 return ExitSuccess


-------------------------------------------------------------------------------
-- etc

-- shell_ cmd      = shell cmd      empty
proc_  cmd args = proc  cmd args empty

cp = Turtle.cp
-- cp = \src dst -> echo (format ("cp ("%fp%") ("%fp%")") src dst) *> return ExitSuccess -- dry-run

sed = Turtle.sed
-- sed = \pat inp -> return ExitSuccess

fp2txt = format fp
-- txt2fp = fromString . T.unpack

git args = proc_ "git" args

(-:) = (,)
bimapPair f g = map (\(a,b) -> (f a, g b))

withPWD newDir mx = do
 oldDir <- pwd
 cd newDir
 x <- mx
 cd oldDir
 return x

-- runhaskell ./update.hs
-- cabal install --user turtle
-- https://hackage.haskell.org/package/turtle

-------------------------------------------------------------------------------
