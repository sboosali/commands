{-# LANGUAGE DeriveAnyClass, LambdaCase, FlexibleContexts #-}
module Commands.Plugins.Spiros.Emacs.Run where 
import           Commands.Plugins.Spiros.Emacs.Types
import           Commands.Plugins.Spiros.Emacs.Config
import           Commands.Plugins.Spiros.Extra 
import           Commands.Plugins.Spiros.Phrase

import Commands.Backends.Workflow


insertMarking :: MonadWorkflow m => String -> m() 
insertMarking s = do 
 mark
 insert s 

insertHighlighting :: MonadWorkflow m => String -> m() 
insertHighlighting s = do 
 mark
 delay emacsDelay 
 insert s 
 delay emacsDelay 
 activate_mark

runEmacs_ :: MonadWorkflow m => Emacs -> m() 
runEmacs_ = \case 
 EmacsFunction   Nothing  -> execute_extended_command
 EmacsFunction   (Just p) -> runEmacs =<< munge p
 EmacsExpression Nothing  -> eval_expression
 EmacsExpression (Just p) -> evalEmacs =<< munge p

{- | generates actions to evaluate a stringly-typed s-expression in Emacs, just like @M-:@.

since it opens a minibuffer in Emacs, it needs @(setq enable-recursive-minibuffers t)@ to work when the current buffer is already a minibuffer.

-}
evalEmacs :: MonadWorkflow m => ElispSexp -> m ()
evalEmacs sexp = do
 eval_expression
 slot sexp

-- parseSexp :: String -> Possibly ElispSexp
-- parseSexp = undefined

-- prettySexp :: ElispSexp -> String
-- prettySexp = undefined

{- | generates actions to execute an interactive command in Emacs, just like @M-x@.

a pseudo-rpc for Emacs:

* "rpc" because you can call emacs commands (I.e. interactive functions) with arguments

* "pseudo" because the return type is unit: the communication is via one-way keyboard-shortcuts, rather than a two-way channel like a network connection.

no "type"-checking or arity-checking.

since it opens a minibuffer in Emacs, it needs @(setq enable-recursive-minibuffers t)@ to work when the current buffer is already a minibuffer.

-}
runEmacsWith
 :: MonadWorkflow m
 =>  String                      --  ^ the name of the interactive command
 -> [String]                    --  ^ the arguments that would be manually entered, one at a time, in a minibuffer
 -> m ()
runEmacsWith f xs = do
 execute_extended_command -- TODO non-standard: make this configurable? ImplicitParams?
 slot f
 traverse_ slot xs
-- configurable by actions being a ReaderT? The solved another problem, delays or something.
-- or just let the user define it, after copying and pasting the Example Plug-in. That's the whole point of the configuration being Haskell.
--
-- integrate with a vocabulary. or simple sum grammar, falling back to dictation.
-- print a list of all interactive commands, tokenize by splitting on "-".
-- http://stackoverflow.com/questions/29953266/emacs-list-the-names-of-every-interactive-command

-- | like 'runEmacsWith', but takes no arguments.
runEmacs :: MonadWorkflow m => String -> m ()
runEmacs f = runEmacsWith f []

-- | like 'runEmacs', but doesn't press enter. 
runEmacsWait :: MonadWorkflow m => String -> m ()
runEmacsWait f = do
 execute_extended_command
 insert f

-- | like 'runEmacsWith', but takes string-returning-actions as arguments.
--
-- e.g. @runEmacsWithA "regexp-search" ['getClipboard']@
runEmacsWithA :: MonadWorkflow m => String -> [m String] -> m ()
runEmacsWithA f as = do
 xs <- traverse id as
 runEmacsWith f xs

-- | like 'runEmacsWith', but takes phrases as arguments.
--
-- e.g. @runEmacsWithP "regexp-search" ['PAtom' 'Pasted']@
runEmacsWithP :: MonadWorkflow m => String -> [Phrase] -> m ()
runEmacsWithP f ps = do
 xs <- traverse munge ps
 runEmacsWith f xs

rankEmacs :: Emacs -> Int
rankEmacs = \case 
 EmacsFunction   Nothing  -> 0
 EmacsFunction   (Just p) -> rankPhrase p
 EmacsExpression Nothing  -> 0
 EmacsExpression (Just p) -> rankPhrase p

