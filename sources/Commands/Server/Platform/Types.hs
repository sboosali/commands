{-# LANGUAGE RecordWildCards #-}

{-|
-}
module Commands.Server.Platform.Types where

import Data.Default.Class

import qualified System.Info
import Prelude.Spiros

import Prelude(error)

{-|
-}
data KnownPlatform = OSX | Windows

{-| @= 'defaultPlatform'@
-}
instance Default KnownPlatform where def = defaultPlatform

{-|
-}
defaultPlatform :: KnownPlatform
defaultPlatform = case System.Info.os of -- System.Info.arch?
  "darwin"  -> OSX
  "mingw32" -> Windows
  s -> error $ "[Data.Default.def @Platform] unknown System.Info.os: " ++ show s --TODO ErrorCall
