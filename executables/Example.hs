{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-matches #-}
import qualified Commands.Plugins.Example     as Commands
import qualified Commands.Servers.Servant.API as Commands
-- import qualified Commands.Backends.OSX.Example as OSX

import           System.Environment           (getArgs)


main = mainWith =<< getArgs

mainWith = \case
 ["serve"] -> do
  print "serveCommands 8666"
  Commands.serveCommands 8666
 _ -> Commands.main
 -- OSX.main
