{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Commands.Plugins.Spiros.Template.QQ where
import           Commands.Plugins.Spiros.Template.Types

import           Text.InterpolatedString.Perl6 (qc)


{-|

@
'haddockTemplate' text = ['qc'|
\{-| {text} {'cursor'}

-}
|]
@

-}
haddockTemplate :: String -> Template
haddockTemplate "" = [qc|
\{-| {cursor}

-}

|]

{-| insert a comment -}
haddockTemplate text = [qc|
\{-| {text} {cursor}

-}
|]

grammarTemplate :: String -> Template
grammarTemplate text = [qc|
{text} :: R _
{text} = '{text} <=> empty
 <|> {cursor}

|]

{-|

-}
grammarModuleTemplate :: String -> String -> Template
grammarModuleTemplate typeName valueName = [qc|
\{-# LANGUAGE TemplateHaskellQuotes, OverloadedStrings, PostfixOperators #-}
\{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
\{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
\{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
\{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Plugins.Spiros.{typeName}  where
import Commands.Plugins.Spiros.Extra

import Commands.Mixins.DNS13OSX9

import Control.Applicative


{valueName} :: R _
{valueName} = '{valueName} <=> empty
 <|> empty{cursor}

|]
