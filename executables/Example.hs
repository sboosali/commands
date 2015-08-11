{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import qualified Commands.Plugins.Example       as Example
import qualified Commands.Plugins.Example.Press as Press
import qualified Commands.Plugins.Example.Root  as Example

import           System.Environment             (getArgs)


--
-- $ sleep 2; curl -d '["replace this with that"]' 'http://localhost:8666/recognition/'


main = mainWith =<< getArgs

mainWith = \case
 ["serve"] -> do
  Example.spirosServer
 -- _ -> Example.main
 _ -> Press.mainPress
