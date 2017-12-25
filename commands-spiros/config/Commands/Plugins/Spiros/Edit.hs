{-# LANGUAGE TemplateHaskellQuotes, OverloadedStrings, RankNTypes, LambdaCase, PostfixOperators, PartialTypeSignatures, TupleSections, FlexibleContexts, NoMonomorphismRestriction  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-name-shadowing #-}  -- fewer type signatures (i.e. more type inference) makes the file more "config-like"
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Edit
 ( module Commands.Plugins.Spiros.Edit
 , module Commands.Plugins.Spiros.Edit.Types
 , module Commands.Plugins.Spiros.Edit.Run
 ) where

import Commands.Plugins.Spiros.Edit.Types
import Commands.Plugins.Spiros.Edit.Run
import Commands.Plugins.Spiros.Extra

import           Commands.Mixins.DNS13OSX9

-- ================================================================ --

move = 'move
 <=> Move   <$> direction <*> region
 <|> MoveTo <$> endpoint  <*> region

endpoint = 'endpoint
 <=> Beginning <$ "beg"
 <|> Ending    <$ "end"

direction = tidyGrammar
-- direction = transformedGrammar (filter (/= '_'))
-- direction = qualifiedGrammarWith "_"

edit = 'edit <=> empty
 -- <|> Edit Cut Forwards Line <$ "kill" -- NOTE overrides
 --     -- i.e. "kill" -> "kill for line", not "kill whole that"
 <|> Edit <$> action              <*> (slice -?- defaultSlice) <*> (region -?- defaultRegion)
    -- e.g. "cop" or "cop that" or "cop whole" -> "cop whole that"
 <|> Edit <$> (action -?- defaultAction) <*> slice             <*> region
    -- e.g. "for line" -> "sel for line"

 -- the "kill" case is why I abandoned parsec: it didn't backtrack sufficiently. we want:
 -- "cop" -> Edit Copy Whole That
 -- "kill" -> Edit Cut Forwards Line, not Edit Cut Whole That
 -- "kill for line" -> Edit Cut Forwards Line, not {unexpected 'f', expecting end of input}

-- TODO maybe RHS should have access to a configuration environment? Oh my.
-- could also provide the keyword (i.e. only literals) feature, rather than forcing it on the parser.

action = 'action <=> empty
 <|> Select      <$ "sell"
 <|> Copy        <$ "copy"      -- "cop"
 <|> Cut         <$ "cut"      -- "kill"
 <|> Delete      <$ "del"
 <|> Transpose   <$ "trans"
 <|> Google      <$ "google"

slice = 'slice <=> vocab
 [ "whole"-:Whole
 , "back"-: Backwards
 , "fuss"-: Forwards
 ]
 -- "for" would be homophone with "four", while both Positive and Slice can be the prefix (i.e. competing for the same recognition).
-- "four kill for word" should be [4 Kill Forwards Word] not [4 Kill Forwards Line, 4 Sel Whole Word]
-- "four kill fuss word" is unambiguously [4 Kill Forwards Word]

region = 'region
 <=> That       <$ "that"
 <|> Character  <$ "char"
 <|> Word_      <$ "word"
 <|> Token      <$ "toke"
 <|> Group      <$ "group"
 <|> Line       <$ "line"
 <|> Rectangle  <$ "wreck"
 <|> Block      <$ "block"
 <|> Page       <$ "page"
 <|> Screen     <$ "screen"
 <|> Everything <$ "all"
 <|> Definition <$ "def"
 <|> Function_  <$ "fun"
 <|> Reference  <$ "ref"
 <|> Structure  <$ "struct"

getHighlighted = selected Whole That
