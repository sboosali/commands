{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import qualified Commands.Plugins.Example       as Example
import qualified Commands.Plugins.Example.Root as Example
import Data.Canonical 

import           System.Environment             (getArgs)


--
-- $ sleep 2; curl -d '["replace this with that"]' 'http://localhost:8666/recognition/'


main = mainWith =<< getArgs

mainWith = \case
 ["serve"] -> do
  Example.exampleServer
 _ -> Example.realMain >> mainCanonical 
