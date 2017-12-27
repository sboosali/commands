{-# LANGUAGE TemplateHaskell, OverloadedStrings, PostfixOperators, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts, LambdaCase #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Plugins.Spiros.Correct.Grammar  where
import Commands.Plugins.Spiros.Digit.Grammar

import Commands.Plugins.Spiros.Phrase

-- import Commands.Server.Types(Correction(..))
import Commands.Mixins.DNS13OSX9

import Digit

import Prelude.Spiros

--------------------------------------------------------------------------------

{-| a user can choose:

* one of the (ten or fewer) hypotheses they are presented with.
* an arbitrary sentence (possibly an edited hypothesis).

(The speech engine might then reject it sounds too different
from the recognition being corrected).

-}
data Correction
 = ChosenCorrection Digit
 | SpokenCorrection Dictation
 | EditedCorrection Digit --TODO to preserve DNSTokens metadata
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable) -- TODO Enumerable 

 {-| the hypotheses of the previous recognition.
 its length is between one and ten.

 -}
newtype Hypotheses = Hypotheses [Hypothesis]
 deriving stock   (Show,Read,Data,Generic)
 deriving newtype (Eq,Ord,Hashable,NFData)

{-|
-}
newtype Hypothesis = Hypothesis [Text]
 deriving stock   (Show,Read,Data,Generic)
 deriving newtype (Eq,Ord,Hashable,NFData)

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------