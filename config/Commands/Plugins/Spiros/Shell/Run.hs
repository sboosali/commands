{-# LANGUAGE LambdaCase, OverloadedStrings #-} 
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Commands.Plugins.Spiros.Shell.Run where 
import           Commands.Plugins.Spiros.Shell.Types 
import           Commands.Plugins.Spiros.Rank 
import           Commands.Plugins.Spiros.Extra 
import           Commands.Plugins.Spiros.Phrase


instance (Rankable Shell) where rank = rankShell

rankShell :: Ranking Shell
rankShell = \case
 Shell _ _cmd args -> rankPhrase args

runShell :: Desugaring Shell
runShell = \case

 Shell Safe cmd args -> do
  slotP$ word2phrase cmd <> " " <> args <> " " 

 Shell Unsafe cmd args -> do
  insertP$ word2phrase cmd <> " " <> args <> " " 

