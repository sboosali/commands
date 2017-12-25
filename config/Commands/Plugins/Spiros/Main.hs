{-# LANGUAGE LambdaCase, LiberalTypeSynonyms, RankNTypes, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-|

@
stack build && stack exec -- spiros-server
stack build && stack exec -- spiros-server test
stack build && stack exec -- spiros-server finite
stack build && stack exec -- spiros-server derive
stack build && stack exec -- spiros-server parse three up line beg line two del fuss word
@

-}
module Commands.Plugins.Spiros.Main where
-- import Commands.Plugins.Spiros.Server
--TODO import Commands.Plugins.Spiros.Finite.Example

import System.Environment (getArgs)

main = mainWith =<< getArgs

mainWith = \case --TODO
 --
 -- [] -> spirosServer
 --
 -- ["test"] -> spirosTest
 --
 -- -- ["finite"] -> printFinite
 --
 -- ["derive"] -> printClick
 --
 -- ("parse" : ws) -> parseMain ws

 _ -> return()
