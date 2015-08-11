{-# LANGUAGE LambdaCase, LiberalTypeSynonyms, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables                                          #-}
module Commands.Plugins.Example where
import qualified Commands.Backends.OSX         as OSX
import           Commands.Etc
import           Commands.Frontends.Dragon13
import           Commands.Mixins.DNS13OSX9
import           Commands.Plugins.Example.Root
import           Commands.Servers.Servant

import           Control.Lens                  hiding (from, ( # ))
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy.Char8    as BSC
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.IO             as T
import           Servant

import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.ST.Unsafe
import           System.IO.Unsafe


-- ================================================================ --

spirosServer :: IO ()
spirosServer = serveNatlink (spirosSettings rootCommand)
-- spirosServer = spirosServe rootCommand
-- spirosServer = spirosServe rootPlugin

-- rootPlugin :: VPlugin_ r Root
-- rootPlugin = VPlugin rootCommand

-- de'serve :: (Show a) => (VPlugin_ r a) -> IO ()
 -- Couldn't match type ‘VSettings (E.Rule r a) a’ with ‘forall r1. VSettings_ r1 a0’
-- de'serve :: (Show a) => (forall r. VPlugin_ r a) -> IO ()
 -- Couldn't match type ‘VSettings (E.Rule r0 a) a’ with ‘forall r. VSettings_ r a0’
-- de'serve plugin = de'Settings plugin >>= serveNatlink

-- spirosServe :: (Show a) => (forall r. RULED DNSEarleyCommand r a) -> IO ()
-- -- de'serve plugin = unsafePerformIO(de'Settings plugin) & serveNatlink
-- spirosServe plugin = serveNatlink (spirosSettings plugin)

spirosSettings :: (Show a) => RULED DNSEarleyCommand r a -> RULED VSettings r a
spirosSettings command = VSettings 1337 spirosSetup spirosInterpret (spirosUpdateConfig command)

-- spirosSettings :: forall r a. VPlugin_ r a -> (VSettings_ r a)
-- spirosSettings plugin = (defSettings runActions spirosUpdateConfig plugin)
--   { vSetup = spirosSetup
--   }

-- spirosSettings :: forall r. VPlugin (E.Rule r Root) Root -> IO (VSettings (E.Rule r Root) Root)
-- spirosSettings plugin = do
--  settings :: (VSettings (E.Rule r Root) Root) <- (defSettings runActions spirosUpdateConfig plugin)
--  return$ settings
--   { vSetup = setupCopyGrammar :: (VSettings (E.Rule r Root) Root -> IO (Either VError ()))
--   }

-- spirosUpdateConfig :: VPlugin (E.Rule r Root) Root -> IO (VConfig (E.Rule r Root) Root)
spirosUpdateConfig :: RULED DNSEarleyCommand r a -> RULED VConfig r a
spirosUpdateConfig command = unsafePerformIO$ do
 vGrammar <- de'deriveGrammarObservedSharing (command&_cRHS)
 -- let eProd = runST$ de'deriveParserObservedSharing (command&_cRHS)
 eProd <- unsafeSTToIO$ de'deriveParserObservedSharing (command&_cRHS) --TODO runST, but input is not rank2
 let vParser = EarleyParser eProd (command&_cBest)
 let vDesugar = (command&_cDesugar)
 return VConfig{..}
{-# NOINLINE spirosUpdateConfig #-}

spirosSetup :: RULED VSettings r a -> IO (Either VError ())
spirosSetup settings = do

 let address = Address (Host "192.168.56.1") (Port (settings&vPort))

 shimSerialize address (settings&vConfig&vGrammar) & \case
  Left e -> do
   return$ Left(VError (show e))
  Right (PythonFile shim) -> do
   putStrLn "" -- TODO logging

   putStrLn$ T.unpack shim
   putStrLn ""

   OSX.runActions$ OSX.setClipboard (T.unpack shim)

   T.putStrLn$ displayAddress address
   putStrLn ""

   return$ Right()

{- | this handler:

* supports short-circuiting (in 'EitherT'), returning an HTTP error status.
* executes the compiled actions (in 'IO').


-}
spirosInterpret :: (Show a) => (forall r. RULED VSettings r a) -> [Text] -> Response ()
spirosInterpret vSettings = \ws -> do
 liftIO$ putStrLn "" >> putStrLn "" >> putStrLn ""

 liftIO$ putStrLn$ "WORDS:"
 liftIO$ T.putStrLn$ T.intercalate (T.pack " ") ws

 value <- e'ParseBest (vSettings&vConfig&vParser) ws & \case
  Left e -> left$ err400{errBody = BSC.pack (show e)}
  Right x -> return x
 liftIO$ putStrLn$ "VALUE:"
 liftIO$ print value

 context <- liftIO$ OSX.runActions OSX.currentApplication
 liftIO$ putStrLn$ "CONTEXT:"
 liftIO$ print context

 let actions = (vSettings&vConfig&vDesugar) context value
 liftIO$ OSX.runActions actions
 liftIO$ putStrLn$ "ACTIONS:"
 liftIO$ putStrLn$ OSX.showActions actions
 return()

