{-# LANGUAGE ViewPatterns, LambdaCase, DeriveAnyClass, NoMonomorphismRestriction #-}
{-| "Commands.Plugins.Spiros.Root" 'Commands.Plugins.Spiros.Root.root'

-}
module Commands.Server.Correction.Shell where
import Commands.Server.Correction.Types
import Commands.Server.Platform.Types

-- import Commands.Frontends.Natlink.Types
import Commands.Frontends.Dictation
import Digit
import Workflow.Core (MonadWorkflow)
import qualified Workflow.Core as W

import qualified Data.List.NonEmpty as NonEmpty
import System.Console.Haskeline
import qualified Data.Text.Lazy                as T

import Data.Monoid              (First(..))

import Prelude.Spiros
import Prelude(error)

--------------------------------------------------------------------------------

{-|
-}
defaultCorrectionSettings :: (MonadIO m, MonadWorkflow m) => CorrectionSettings m
defaultCorrectionSettings = CorrectionSettings{..}
 where
 _promptCorrection  = promptCorrectionViaShell
 _reachCorrectionUi = reachShell
 _leaveCorrectionUi = leaveShell

reachShell :: (MonadWorkflow m) => m ()
reachShell = case def of
  OSX     -> W.openApplication "Terminal"
  Windows -> error "TODO reachShell" -- W.reachApplication "cmd" "Command Prompt" -- TODO window title if already open, executable name if not

leaveShell :: (MonadWorkflow m) => m ()
leaveShell = case def of
   OSX     -> openPreviousApplication
   Windows -> openPreviousApplication

openPreviousApplication :: (MonadWorkflow m) => m ()
openPreviousApplication = case def of
   OSX     -> do
    W.press "M-<tab>"
    W.press "<ret>"
   Windows -> do
    W.press "M-<tab>"

-- openApplication s = reachApplication s s
-- openApplication -- TODO "launchApplication" on OSX, "reach by exe name, or open" on Windows/Linux
--
-- reachApplication win exe = do
--   reachWindow win <+> launchApplication exe -- TODO MonadPlus for failure

--------------------------------------------------------------------------------

{- |
uses @haskeline@, which is xplatform, for navigation by arrow keys.
-}
promptCorrectionViaShell :: (MonadIO m) => Hypotheses -> m Dictation
promptCorrectionViaShell hypotheses = liftIO $ runInputT defaultSettings loop
 where
 loop :: InputT IO Dictation
 loop = do
  getInputLine "correction> " >>= \case

      Nothing   -> error "promptCorrection"  -- TODO Nothing NOTE "if the user pressed Ctrl-D when the input text was empty"
      Just line -> case getCorrection hypotheses line of
          Nothing -> loop
          Just d -> return d

{-|

TODO fix tests

>>> getCorrection (Hypotheses [["hello","world"]]) " "
Nothing
>>> getCorrection (Hypotheses [["hello","world"]]) "0" -- TODO make it abort
Just (Dictation ["hello","world"])
>>> getCorrection (Hypotheses [["hello","world"]]) "1"
Nothing
>>> getCorrection (Hypotheses [["hello","world"]]) "some words"
Just (Dictation ["some","words"])

-}
getCorrection :: Hypotheses -> String -> Maybe Dictation
getCorrection hypotheses line = fromCorrection hypotheses =<< parseCorrection line

parseCorrection :: String -> Maybe Correction
parseCorrection (strip -> s) = getFirst $
 First (ChosenCorrection <$> parseDigit s) <> First (SpokenCorrection <$> isDictation s)
 where
 isDictation = fmap (words2dictation . NonEmpty.toList) . NonEmpty.nonEmpty      -- TODO

fromCorrection :: Hypotheses -> Correction -> Maybe Dictation
fromCorrection hypotheses = \case
 ChosenCorrection i  -> i & indexHypotheses hypotheses & fmap (Dictation . fmap T.unpack)
 SpokenCorrection ws -> Just ws
 EditedCorrection _i -> error "TODO fromCorrection"

indexHypotheses :: Hypotheses -> Digit -> Maybe [Text]
indexHypotheses (Hypotheses hs) (Digit i) = hs `index` i

-- concurrency, other requests shouldn't be buffered, they should be ignored.
-- consistency between the server state and the client state.
-- zero-based, original recognition in parentheses, out of order, OR decrement index no don't
-- haskell flush clear stdin
