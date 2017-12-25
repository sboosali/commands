{-# LANGUAGE TemplateHaskellQuotes, PostfixOperators, FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Emacs.Grammar where
import           Commands.Plugins.Spiros.Emacs.Types
import           Commands.Plugins.Spiros.Extra
-- import           Commands.Plugins.Spiros.Emacs.InteractiveCommands
import           Commands.Plugins.Spiros.Phrase

import           Commands.Mixins.DNS13OSX9

emacs = 'emacs <=> empty
 <|> (EmacsFunction   . fromPasted) <$ "run"  <*> pasted
 <|> EmacsFunction      <$ "run"  <*> (interactive_-?)
 <|> EmacsExpression    <$ "eval" <*> (phrase-?)

 -- <|> EmacsFunction   (Just [Pasted_]) <$ (t"run paste")
 -- <|> EmacsExpression (Just [Pasted_]) <$ (t"eval paste") -- TODO shouldn't be necessary
 -- <|> EmacsFunction      <$ "run"  <*> (interactive_-?)
 -- <|> EmacsExpression    <$ "eval" <*> (phrase_-?)
 where
 -- interactive_ = (word2phrase') <$> interactive   -- not a full phrase, for accuracy
 interactive_ = phrase -- TODO takes fifteen seconds to load a vocabulary of five thousand
 fromPasted = Just . fromPhrase_

myBuffers :: R Phrase
myBuffers = vocab
 [ ""-: ""
--  , "shell"-: "*shell*"
 , ""-: ""
 ]
