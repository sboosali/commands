{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-matches #-}
import qualified Commands.Plugins.Example        as Example
import qualified Commands.Plugins.Example.Phrase as Example
import qualified Commands.Servers.Servant        as Server
-- import qualified Commands.Backends.OSX.Example as OSX

import           System.Environment              (getArgs)

--
-- $ sleep 2; curl -d '["replace this with that"]' 'http://localhost:8666/recognition/'


main = mainWith =<< getArgs

mainWith = \case
 ["serve"] -> do
  print "serveCommands 8666 root"
  Server.serveNatlink 8666 theModel
 _ -> Example.main
 -- OSX.main

theModel :: Server.CmdModel z Example.Root
theModel = (Server.CmdModel Example.root (Example.Phrase_ . Example.asPhrase) "emacs")
