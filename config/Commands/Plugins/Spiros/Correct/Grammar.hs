{-# LANGUAGE TemplateHaskell, OverloadedStrings, PostfixOperators, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Plugins.Spiros.Correct.Grammar  where
import Commands.Plugins.Spiros.Digit.Grammar

import Commands.Plugins.Spiros.Phrase

import Commands.Server.Types(Correction(..))
import Commands.Mixins.DNS13OSX9

import Digit

import Control.Applicative

correctionGrammar :: R Correction -- TODO, upon grammatical contexts
correctionGrammar = 'correctionGrammar <=> empty
 <|> ChosenCorrection <$> digit_
 <|> (SpokenCorrection . letters2dictation) <$ "spell" <*> letters
 <|> SpokenCorrection <$> dictation
 <|> EditedCorrection <$ "edit" <*> digit_

runCorrection = \case
 ChosenCorrection i -> runDigit i
 SpokenCorrection d -> runDictation d
 EditedCorrection _i -> error "runCorrection" --TODO Must live in some CommandsServerT monad, Not just a Workflow. This lets the user configure their own correction UI, without changing the server source.
 where
 runDigit = insertD . digit2dictation

digit2dictation :: Digit -> Dictation
digit2dictation (Digit d) = words2dictation . show $ d
