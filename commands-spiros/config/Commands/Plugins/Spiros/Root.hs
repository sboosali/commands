{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE LiberalTypeSynonyms, NamedFieldPuns, PartialTypeSignatures     #-}
{-# LANGUAGE PatternSynonyms, PostfixOperators, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, OverloadedStrings, TupleSections            #-}
{-# LANGUAGE ViewPatterns                                                   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-name-shadowing -fno-warn-missing-pattern-synonym-signatures #-}  -- fewer type signatures (i.e. more type inference) makes the file more "config-like"
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Root
 ( module Commands.Plugins.Spiros.Root
 , module Commands.Plugins.Spiros.Root.Types
 , module Commands.Plugins.Spiros.Root.Run -- TODO there should be a .Grammar that is imported, rather than vice versa
 ) where
import           Commands.Plugins.Spiros.Extra
import           Commands.Plugins.Spiros.Module (SpirosCommand) -- , SpirosParser)
import           Commands.Plugins.Spiros.Root.Types
import           Commands.Plugins.Spiros.Root.Run
import           Commands.Plugins.Spiros.Act.Grammar
import           Commands.Plugins.Spiros.Macros
import           Commands.Plugins.Spiros.Emacs
import           Commands.Plugins.Spiros.Phrase
-- import           Commands.Plugins.Spiros.Correct.Grammar
import           Commands.Plugins.Spiros.Digit.Grammar
import           Commands.Plugins.Spiros.Shortcut
import           Commands.Plugins.Spiros.Shell
import           Commands.Plugins.Spiros.Edit
-- import  Commands.Plugins.Spiros.Keys

import           Commands.Mixins.DNS13OSX9
-- import           Commands.Parsers.Earley              (EarleyParser(..))
import Digit

--------------------------------------------------------------------------------

-- --rootsCommand :: SpirosCommand
-- rootsCommand = Command{..}
--  where
--  _cRHS     = roots
--  _cBest    = bestRoots
--  _cDesugar = runRoots
--
-- rootsParser :: SpirosParser s r Roots
-- rootsParser = EarleyParser (unsafeEarleyProd roots) bestRoots -- TODO rankRoots
--
-- roots :: R Roots
-- roots = 'roots <=> empty
--  <|> freezeRoot
--  <|> Ambiguous <$ (fromString RootsAmbiguousPrefix) <*> root --TODO recursion
--  <|> Macro_     <$> (number-?-1) <*> myMacros
--  <|> Macro_ 1   <$  "my"    <*> myAliases -- TODO
--  <|> Root_ <$> root
--
-- freezeRoot :: R Roots
-- freezeRoot = 'freezeRoot <=> empty --TODO recursion
--  <|> Frozen [RawStage]    <$ (fromString RootsFrozenPrefix) <* "raw"   <*> root -- TODO doesn't work
--  <|> Frozen [ParseStage]  <$ (fromString RootsFrozenPrefix) <* "parse" <*> root -- TODO doesn't work
--  <|> Frozen constructors  <$ (fromString RootsFrozenPrefix)            <*> root
--
-- pattern RootsAmbiguousPrefix = "explicate"
-- pattern RootsFrozenPrefix    = "freeze"

-------------------------------------------------

rootCommand :: SpirosCommand
rootCommand = Command{..}
 where
 _cRHS     = root
 _cBest    = bestRoot
 _cDesugar = runRoot

root :: R Root
root = 'root <=> empty
  <|> Macro_     <$> (number-?-1) <*> myMacros
  <|> Macro_ 1   <$  "my"    <*> myAliases -- TODO
  <|> Root_ <$> root_

root_ :: R Root_
root_ = 'root_ <=> empty
 <|> Acts_      <$> (acts-++)
 <|> Emacs_     <$> (number-?-1) <*> emacs
 <|> Shortcut_  <$> (number-?-1) <*> myShortcuts
 <|> Shell_     <$>                  shell
 <|> Dictation_ <$  "say"   <*> dictation
 <|> (Dictation_ . word2dictation ) <$  "way" <*> word_ 
 <|> Letters_   <$  "spell" <*> letters
 <|> (Dictation_ . digit2dictation) <$> digit_
 -- <|> Correction_ <$ correction
 -- <|> Click_ <$> click

 <|> Phrase_    <$  "pray" <*> phrase  --TODO why not?
 <|> Phrase_    <$> phrase  -- must be last, phrase falls back to wildcard.

click = 'click <=> empty
 <|> Click <$> (times-?-Single) <*> (button-?-LeftButton) <* "click"

times = enumGrammar

button = qualifiedGrammar

--------------------------------------------------------------------------------

--TODO
digit2dictation :: Digit -> Dictation
digit2dictation (Digit d) = words2dictation . show $ d
