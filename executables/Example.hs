{-# LANGUAGE FlexibleInstances, LambdaCase, TupleSections, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-orphans #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
import qualified Commands.Plugins.Example as Example
-- import qualified Commands.Plugins.Example.Phrase as Example
-- import qualified Commands.Servers.Servant as Server
-- import qualified Commands.Backends.OSX.Example as OSX
-- import           Commands.Plugins.Example.Emacs
-- import qualified Data.RefCache            as RefCache

-- import           Data.Reify
import           System.Environment       (getArgs)
-- import Data.Unique
-- import           Control.Monad
-- import           Control.Monad.ST
-- import           Control.Monad.ST.Unsafe
-- import           Data.Functor.Identity
-- import           Data.IORef
-- import           Data.STRef


--
-- $ sleep 2; curl -d '["replace this with that"]' 'http://localhost:8666/recognition/'


main = mainWith =<< getArgs

mainWith = \case
 ["serve"] -> do
  Example.rootServe
 _ -> Example.main
 -- _ -> mainEmacs
 -- _ -> OSX.main

-- rootServer :: IO ()
-- rootServer = Example.de'Settings Example.rootPlugin >>= Server.serveNatlink
