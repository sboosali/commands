{-# LANGUAGE DeriveAnyClass, TemplateHaskell, PostfixOperators, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Act.Grammar where
import           Commands.Plugins.Spiros.Act.Types
import           Commands.Plugins.Spiros.Edit
import           Commands.Plugins.Spiros.Number
import           Commands.Plugins.Spiros.Keys

import           Commands.Mixins.DNS13OSX9

import           Control.Applicative


acts = 'acts
 <=> ActsRW <$> (number-?-1) <*> act

act = 'act <=> empty     -- boilerplate (mostly)
 <|> KeyRiff_ <$> keyriff
 --TODO <|> Click_   <$> click
 <|> Edit_    <$> edit
 <|> Move_    <$> move

-- acts = 'acts
--  <=> ActRW <$> (number-?-1) <*> actRW
--  <|> ActRO <$> actRO

-- actRO = 'actRO <=> empty -- TODO

-- actRW = 'actRW <=> empty
--  <|> act
