{-# LANGUAGE LambdaCase, LiberalTypeSynonyms, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables                                          #-}
module Commands.Plugins.Example where
import           Commands.Plugins.Example.Root

import qualified Commands.Backends.OSX         as OSX
import           Commands.Etc
import           Commands.Frontends.Dragon13
import           Commands.Mixins.DNS13OSX9
import           Commands.Servers.Servant

import           Control.Lens                  hiding (from, ( # ))
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy.Char8    as BSC
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.IO             as T
import           Servant
import Data.Time

import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad
import           Control.Monad.ST.Unsafe
import           System.IO.Unsafe
import System.Mem

-- ================================================================ --

exampleServer :: IO ()
exampleServer = serveNatlink (exampleSettings rootCommand)
-- exampleServer = exampleServe rootCommand
-- exampleServer = exampleServe rootPlugin

-- rootPlugin :: VPlugin_ r Root
-- rootPlugin = VPlugin rootCommand

-- de'serve :: (Show a) => (VPlugin_ r a) -> IO ()
 -- Couldn't match type ‘VSettings (E.Rule r a) a’ with ‘forall r1. VSettings_ r1 a0’
-- de'serve :: (Show a) => (forall r. VPlugin_ r a) -> IO ()
 -- Couldn't match type ‘VSettings (E.Rule r0 a) a’ with ‘forall r. VSettings_ r a0’
-- de'serve plugin = de'Settings plugin >>= serveNatlink

-- exampleServe :: (Show a) => (forall r. RULED DNSEarleyCommand r a) -> IO ()
-- -- de'serve plugin = unsafePerformIO(de'Settings plugin) & serveNatlink
-- exampleServe plugin = serveNatlink (exampleSettings plugin)

exampleSettings :: (Show a) => RULED DNSEarleyCommand r a -> RULED VSettings r a
exampleSettings command = VSettings 8888 exampleSetup exampleInterpret (exampleUpdateConfig command)

-- exampleSettings :: forall r a. VPlugin_ r a -> (VSettings_ r a)
-- exampleSettings plugin = (defSettings runActions exampleUpdateConfig plugin)
--   { vSetup = exampleSetup
--   }

-- exampleSettings :: forall r. VPlugin (E.Rule r Root) Root -> IO (VSettings (E.Rule r Root) Root)
-- exampleSettings plugin = do
--  settings :: (VSettings (E.Rule r Root) Root) <- (defSettings runActions exampleUpdateConfig plugin)
--  return$ settings
--   { vSetup = setupCopyGrammar :: (VSettings (E.Rule r Root) Root -> IO (Either VError ()))
--   }

-- exampleUpdateConfig :: VPlugin (E.Rule r Root) Root -> IO (VConfig (E.Rule r Root) Root)
exampleUpdateConfig :: RULED DNSEarleyCommand r a -> RULED VConfig r a
exampleUpdateConfig command = unsafePerformIO$ do
 vGrammar <- de'deriveGrammarObservedSharing (command&_cRHS)
 -- let eProd = runST$ de'deriveParserObservedSharing (command&_cRHS)
 eProd <- unsafeSTToIO$ de'deriveParserObservedSharing (command&_cRHS) --TODO runST, but input is not rank2
 let vParser = EarleyParser eProd (command&_cBest)
 let vDesugar = (command&_cDesugar)
 return VConfig{..}
{-# NOINLINE exampleUpdateConfig #-}

exampleSetup :: RULED VSettings r a -> IO (Either VError ())
exampleSetup settings = do

 let address = Address (Host "192.168.56.1") (Port (settings&vPort))

 applyShim getShim address (settings&vConfig&vGrammar) & \case
  Left e -> do
   putStrLn$ (show e)
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
exampleInterpret :: (Show a) => (forall r. RULED VSettings r a) -> [Text] -> Response ()
exampleInterpret vSettings = \ws -> do

 t0<- liftIO$ getCurrentTime
 value <- e'ParseBest (vSettings&vConfig&vParser) ws & \case
  Right x -> return x
  Left e -> do
   liftIO$ do
    replicateM_ 3 (putStrLn"")
    putStrLn$ "ERROR:"
    print e
    putStrLn$ "WORDS:"
    T.putStrLn$ T.intercalate (T.pack " ") ws
   left$ err400{errBody = BSC.pack (show e)}

 t1<- liftIO$ getCurrentTime

 context <- liftIO$ OSX.runActions OSX.currentApplication

 let actions = (vSettings&vConfig&vDesugar) context value
 liftIO$ OSX.runActions actions
 t2<- liftIO$ getCurrentTime

 liftIO$ do
  replicateM_ 3 (putStrLn"")
  putStrLn$ "ACTIONS:"
  putStr  $ OSX.showActions actions
  putStrLn ""
  putStrLn$ "TIMES:"
  putStrLn$ show (1000 * (t1 `diffUTCTime` t0))
  putStrLn$ show (1000 * (t2 `diffUTCTime` t1))
  putStrLn ""
  putStrLn$ "CONTEXT:"
  print context
  putStrLn ""
  putStrLn$ "VALUE:"
  print value
  putStrLn ""
  putStrLn$ "WORDS:"
  T.putStrLn$ T.intercalate (T.pack " ") ws
--  performMajorGC
  performMinorGC
