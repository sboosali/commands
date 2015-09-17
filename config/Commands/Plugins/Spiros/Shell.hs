{-# LANGUAGE TemplateHaskell, PostfixOperators, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Shell where 
import           Commands.Plugins.Spiros.Phrase
import           Commands.Plugins.Spiros.Etc

import           Commands.Etc
import           Commands.Mixins.DNS13OSX9

import           GHC.Exts                          (IsString (..))
import Data.Monoid                           ((<>))
-- import           Control.Applicative


data Shell
 = Shell String Phrase'
 deriving (Show,Eq,Ord)

shell = foldMap go (filterBlanks shellCommands)
 where
 go (spoken,written) = Shell <$> (written <$ token spoken) <*> (phrase_-?-blankPhrase)

shellCommands =
 [ "list"-: "ls"
 , "remove"-: "rm"
 , "make dear"-: "mkdir"
 , "remove dear"-: "rmdir"
 , "get"-: "git"
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""

 , both "cabal"
 , both "git"
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""

 ] 


-- ================================================================ --

instance (Rankable Shell) where rank = rankShell

rankShell :: Ranking Shell
rankShell = \case
 Shell _cmd args -> rankPhrase args

runShell :: Desugaring Shell
runShell = \case
 Shell cmd args -> do
  insertP$ [fromString cmd] <> args

