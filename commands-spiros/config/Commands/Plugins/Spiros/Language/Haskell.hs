{-# LANGUAGE NoImplicitPrelude, TemplateHaskellQuotes, OverloadedStrings, PostfixOperators #-}
{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-|

TODO "the import statements header" is a contiguous region,
"every declaration" is a noncontiguous set of regions.
you can navigate between regions,
and you can perform actions on regions (e.g. jump to the start of it, copy it, et cetera ), just like anything highlighted

-}
module Commands.Plugins.Spiros.Language.Haskell  where
import Commands.Plugins.Spiros.Extra

import Commands.Mixins.DNS13OSX9

import Control.Applicative

import Prelude.Spiros

derivingStatement = "deriving" -: derivingStrategy

derivingStrategy = ["stock", "newtype", "anyclass"]

haskellDeclaration = ["data", "newtype", "type", "class", "instance"]
