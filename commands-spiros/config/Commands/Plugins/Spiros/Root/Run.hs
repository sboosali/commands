{-# LANGUAGE ImplicitParams, LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms, NamedFieldPuns, PartialTypeSignatures     #-}
{-# LANGUAGE PatternSynonyms, PostfixOperators, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, OverloadedStrings, TupleSections            #-}
{-# LANGUAGE ViewPatterns,  FlexibleContexts                       #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-name-shadowing #-}  -- fewer type signatures (i.e. more type inference) makes the file more "config-like"
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Root.Run where
import           Commands.Plugins.Spiros.Extra
import           Commands.Plugins.Spiros.Types
import           Commands.Plugins.Spiros.Root.Types
import           Commands.Plugins.Spiros.Act
import           Commands.Plugins.Spiros.Emacs
import           Commands.Plugins.Spiros.Macros (rankMacro, runMacro)
import           Commands.Plugins.Spiros.Phrase
import           Commands.Plugins.Spiros.Shell
-- import           Commands.Plugins.Spiros.Edit
import           Commands.Plugins.Spiros.Shortcut.Types

import Commands.Backends.Workflow as W

-- import Control.Monad.Free.Church (F)
import Control.Lens((^?))

-- import           Control.Parallel


bestRoot = argmax rankRoot

rankRoot = \case                --TODO fold over every field of every case, normalizing each case
 -- Frozen _ r -> highRank + rankRoot r
 -- Ambiguous r -> highRank + rankRoot r
 Macro_ _i m        -> 3 * highRank + rankMacro m
 Root_ r   -> rankRoot_ r

-- prioritize Action over Macro
-- ideally, prioritize "more specific" over "less specific" matches
rankRoot_ = \case
 Acts_ ass            -> 4 * highRank + safeAverage (fmap rankActs ass)
   -- NOTE "google word" now matches the Action, not the Macro
 Shortcut_ _i _s    -> 3 * highRank
 Shell_ s           -> highRank + rankShell s
 Emacs_ _i e        -> highRank + rankEmacs e
 Letters_ _l         -> highRank + 1
 Dictation_ _d       -> highRank + 0
 Phrase_    p        -> rankPhrase p

-- ================================================================ --

runRoot :: SpirosContext -> Root -> SpirosMonad_ -- TODO access context from within the SpirosMonad_
runRoot context = \case
 -- Frozen _ _ -> nothing           -- TODO needs magic server actions , which needs a more general monad stack
 -- Ambiguous _ -> nothing         -- TODO needs magic server actions , which needs a more general monad stack
 Macro_ n f    -> runRepeat (contextualDelay context) n (runMacro f)
 Root_ r  -> runRoot_ context r
--    _ -> pretty print the tree of commands, both as a tree and as the flat recognition,
--  (inverse of parsing), rather than executing. For debugging/practicing, and maybe for batching.

runRoot_ context = \case
 Acts_ ass     -> traverse_ (runActs context) ass      -- no delay
 Shortcut_ n s -> runRepeat (contextualDelay context) n (runShortcut s)
 Shell_ s      -> runShell s
 Emacs_ n e   -> whenJust (context ^? _EmacsContext) $ runRepeat emacsDelay n (runEmacs_ e)
 Letters_ l   -> runLetters l
 Dictation_ d -> runDictationTop context d
 Phrase_ p    -> runPhraseTop context p

contextualDelay = \case
 EmacsContext -> emacsDelay
 ChromeContext -> browserDelay
 _                     -> defaultDelay

-- |
runPhraseByClipboard _context p = do
 s <- (<>" ") <$> munge p
 insertByClipboard s

-- | top level TODO actual spacing
runPhraseTop context p = do
 s <- (<>" ") <$> munge p
 insertGiven context s

-- | top level TODO actual spacing
runDictationTop context d = do
 let s = mungeDictation (spacedDictation d)
 insertGiven context s

-- |
insertGiven context s = do
 ifJust (context ^? _EmacsContext) (insertEmacs s) (insertDefault s)

insertDefault = insert

insertEmacs = insertMarking
-- insertEmacs = insertHighlighting
