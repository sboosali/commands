{-# LANGUAGE TemplateHaskell, OverloadedStrings, RankNTypes, LambdaCase, PostfixOperators, PartialTypeSignatures, TupleSections, FlexibleContexts, NoMonomorphismRestriction  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-name-shadowing #-}  -- fewer type signatures (i.e. more type inference) makes the file more "config-like"
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing

{-| a finite subset of my grammar:

* no dictation (or any "infinite"/"recursive" productions). instead of "camel <dictation>", we must say "camel <region>"; e.g. instead of "camel some words", first dictate "some words" with siri or google speech, then say "camel that" to camel case it. which needs support for identifying the previous dictation, or even the most recently typed text.
* number gets shrunk to a digit

a finite grammar can be "un-associated" into a "vocabulary" (i.e. list of phrases). and thus, can customize weaker speech engines, like NSSpeechRecognizer.

-}
module Commands.Plugins.Spiros.Finite.Grammar where

import Commands.Plugins.Spiros.Extra()
import Commands.Plugins.Spiros.Finite.Types
import Commands.Plugins.Spiros.Edit -- .Grammar

import           Commands.Mixins.DNS13OSX9

import           Control.Applicative


finite = 'finite <=>
 Finite <$> multiple <*> finite0

finite0 = 'finite0 <=> empty
 <|> Edit0    <$> edit
 <|> Move0    <$> move
-- <|> act -- {keyriff}'s implentation is infinite, but actually it's finite (the powerset of the modifiers is only like 2^5)

multiple = 'multiple <=>
 [ "two"-: 2
 , "three"-: 3
 , "four"-: 4
 , "five"-: 5
 ]

{-

Infinite imports finite?

Move
Edit

Separator
Cacing
Joiner
Brackets
Splitter

Pasted
(Finite)Character = Punctuation|Numeric|Phonetic

(Finite)Number = ≤Digit

(Finite)keychord = {1-3}Modifier|Key

myShortcuts

(Finite)myTemplates = haddockTemplate

(Finite)shell = safeShellCommands|unsafeShellCommands

(Finite)myMacros = myMacros0

Naming: shell0?
the "0" means "nullary production" i.e. takes no "recognized arguments"

root = 'root <=> empty
 <|> Acts_      <$> (acts-++)
 <|> Emacs_     <$> (number-?-1) <*> emacs
 <|> Shortcut_  <$> (number-?-1) <*> myShortcuts
 <|> Shell_     <$>                  shell
 <|> Dictation_ <$  "say"   <*> dictation
 <|> Letters_   <$  "spell" <*> letters
 <|> (Dictation_ . digit2dictation) <$> digit_
 -- <|> Correction_ <$ correction 
 -- <|> Click_ <$> click

act = 'act <=> empty     -- boilerplate (mostly)
 <|> KeyRiff_ <$> keyriff
 --TODO <|> Click_   <$> click
 <|> Edit_    <$> edit
 <|> Move_    <$> move
 
data Roots
 = Root_ Root
 | Macro_      Number  Macro  -- ^ repeated 
 deriving (Show,Eq,Ord)

data Root
 = Acts_       [Acts]         -- ^ chained and repeated
 | Shortcut_   Number  Shortcut  -- ^ repeated 
 | Shell_              Shell  -- ^
 | Emacs_      Number  Emacs  -- ^ repeated 
 | Letters_    Letters       -- ^ 
 | Dictation_  Dictation     -- ^ 
 | Phrase_     Phrase        -- ^ 
 deriving (Show,Eq,Ord,Generic,Data,NFData)  -- TODO no instance for ,Generic,Data, because Macro is a GADT 

data Acts
 = ActsRW Int Act   -- ^ read/write actions
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData)

data Act
 = KeyRiff_ KeySequence
 --TODO | Click_ Click
 | Edit_ Edit
 | Move_ Move
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData)

e.g. { acts+ }
three up line beg line two del fuss word
[ ActsRW 3 (Move_ (Move Up_ Line))
, ActsRW 1 (Move_ (MoveTo Beginning Line))
, ActsRW 2 (Edit_ (Edit Delete Forwards Word_))
]

TODO first Parse was really:
[ ActsRW 3 (Move_ (Move Up_ Line))
, ActsRW 1 (Move_ (MoveTo Beginning Line))
, ActsRW 2 (Edit_ (Edit Delete Whole That))
, ActsRW 1 (Edit_ (Edit Select Forwards Word_))
]

-}
