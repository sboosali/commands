{-# LANGUAGE DeriveAnyClass, TemplateHaskell, PostfixOperators, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Shell.Grammar where
import           Commands.Plugins.Spiros.Shell.Types
import           Commands.Plugins.Spiros.Phrase
import           Commands.Plugins.Spiros.Extra

import           Commands.Mixins.DNS13OSX9 
import Prelude()
import Prelude.Spiros


shell = 'shell <=> foldMap go shellCommands
 where
 shellCommands =  fmap (leftAppend Safe)   (filterBlanks safeShellCommands)
               ++ fmap (leftAppend Unsafe) (filterBlanks unsafeShellCommands)
 go (safety,spoken,written) = Shell safety <$> (written <$ token spoken) <*> (phrase-?-"")
 leftAppend a (b,c) = (a,b,c)

safeShellCommands =
 [ "list"-: "ls"
 , "make dear"-: "mkdir"
 , "get"-: "git"
 , "CD"-: "cd"
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
 , both "find"
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

 ] ++ concat
 [ fmap (("stack "++) > both)
   [ "build"
   , "exec"
   ]
 ]

unsafeShellCommands =
 [ "remove"-: "rm"
 , "recursively remove"-: "rm -r"
 , "remove dear"-: "rmdir"
 , "are sink"-: "rsync -av"
 , "move"-: "mv"
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
