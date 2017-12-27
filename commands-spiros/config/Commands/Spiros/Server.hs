{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes, FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns, RecordWildCards, PartialTypeSignatures, DoAndIfThenElse #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-type-defaults #-}

{-|

Concept: Vocabulary list (Recognize one of) versus vocabulary buffer (Recognize any Contiguous subsequence)
A list can be a Production That's nested within Any grammar, But a buffer is its own grammar; This is a technical limitation, Rather than a necessary distinction.

A very simple shim, That must only be "installed" (write a file, or copy paste the contents into an editor)
Once (and whenever it changes, bbut not whenever the grammar changes).
all further communication Is networked, aand further  modifications  Can be bootstrapped.

Concept: the Phrase. An important Production That enables  Dictating complexly-formatted sentences/expressions Via a single utterance.
It interleaves arbitrary dictation with keywords. Examples of formatting include: Camel casing, capitalization, spacing, etc.


in Atom, or paredit mode in Emacs, only insert the left grouping character (e.g. "("),
since the right grouping character (e.g. ")") is inserted automatically and the cursor moved back

TODO the sounds of typing on the keyboard is recognized as "up".

TODO on my correction interface, use monospaced and large font ,
which makes it easy to click on visual incorrect characters

-}
module Commands.Spiros.Server where
import Commands.Plugins.Spiros hiding (mainWith)

import "commands-server-simple" Commands.Servers.Simple hiding (Recognition)
-- import Workflow.<platform>
import qualified Commands.Backends.Workflow as Windows
import qualified Workflow.Windows.Bindings as Windows2

import Commands.Frontends.Dragon13 as DNS
import Commands.Frontends.Natlink.Types
--import qualified Commands.Frontends.Dragon13 as DNS
import           Commands.Parsers.Earley (EarleyParser(..), bestParse) -- , eachParse)
import           Commands.Mixins.DNS13OSX9 -- (unsafeDNSGrammar, unsafeEarleyProd)
import qualified Workflow.Core         as W

import           Control.Lens
import           Data.Text.Lazy                        (Text)
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.IO             as T
import "clock" System.Clock 
import "time" Data.Time.Clock 
import "deepseq" Control.DeepSeq (NFData, force) 

import Data.Char
import qualified Data.List as List
import           Control.Monad
import System.IO
import System.Mem
import Prelude()
import "base" Control.Concurrent (forkIO) 
import "base" Control.Exception  (evaluate)

import "spiros" Prelude.Spiros (delayMilliseconds) 

{-|

If you get this error:

@
URLError: <urlopen error [Errno 10061] No connection could be made because the target machine actively refused it>

or

URLError: <urlopen error [Errno 10060] A connection attempt failed because the connected party did not properly respond after a period of time, or established connection failed because connected host has failed to respond>
@

You can fix it by:

Googling "Internet Explorer proxy settings" to find http://www.lib.berkeley.edu/using-the-libraries/proxy-ie10-11-windows

127.0.0.0 v 127.0.0.1 v localhost

TODO Document  fixing this connection refusal   error In more places

the python documentation for urllib.urlopen:
"""
In a Windows environment, if no proxy environment variables are set,
proxy settings are obtained from the registry's Internet Settings
section.
"""

ipconfig /flushdns And restart ??

Open Start Menu and click Control Panel
Click Security (or if you are in classic view click Windows Firewall and goto step 4)
Click 'Allow a program through windows firewall'
Click Change Settings
Click the Exceptions tab
Ensure there is a check aside World Wide Web Services (HTTP)

127.0.0.0 times out, 127.0.0.1 Is actively refused, and localhost Is not found??
but http://127.0.0.1:8888/test still works from browser

whitelist localhost

Fix: (A simpler or automated solution probably exists)
edit the Windows firewall Settings to white list localhost connections from the current user.
E.g.

NOTE on Windows (for reproducibility: Windows ten, Dragon NaturallySpeaking thirteen ,
GHC eight, the server is started by running the executable from a command prompt
(IE normally)), sometimes the server seems to freeze. NatSpeak works,
as does the NatLink shim, but nothing is output to the terminal and no actions are
performed;until the command prompt window that is running the server
receives input (for example, hit enter); this often happens near the start of
the server running, and frequently after navigating away from the window.
TODO: see if running it without a window helps (or if it prevents the hack that fixes
it); also see if the issue is with standard input , maybe the buffering settings
(I have tried line buffering, which I think is the default buffering, and no buffering)
or concurrency(the server is being run with `-threaded`).

-}
main = do
  --
  hSetBuffering stdin LineBuffering -- TODO NoBuffering ? 
  hSetBuffering stdout NoBuffering

  b <- Windows2.enableDebugPriv
  putStrLn $ "enableDebugPriv: " ++ show b

  mainWith myEnvironment

myEnvironment :: _
myEnvironment = VEnvironment myPlugin myNatlink

myPlugin :: _
myPlugin = spirosUpdate
  (defaultDnsOptimizationSettings & dnsOptimizeSerialization.serializationForceWordWrap .~ True)
  rootCommand

myNatlink = NatLinkSettings{..} -- "" -- Todo remove context  fields From NatLinkConfig
  where
  nlAddress = Address (Host "127.0.0.1") (Port 8888) -- Todo Write-once The simple-server's settings&Port
  nlLocation = "C:/NatLink/NatLink/MacroSystem/_commands.py"
  -- inDirectory = ("C:/NatLink/NatLink/MacroSystem/"++)

data NatLinkSettings = NatLinkSettings
 { nlAddress :: Address
 , nlLocation :: FilePath  -- todo Add a field to Config
 }  deriving (Show,Read,Eq,Ord,Data,Generic)

defaultNatlinkSettings :: NatLinkSettings
defaultNatlinkSettings = NatLinkSettings{..} -- "" -- Todo remove context  fields From NatLinkConfig
  where
  nlAddress = Address (Host "127.0.0.1") (Port 8888) -- Todo Write-once The simple-server's settings&Port
  nlLocation = "C:/NatLink/NatLink/MacroSystem/_commands.py"

{-| the \"diff\" of an 'Invocation'.

-}
data Invoker m a = Invoker
  { iParse   :: [Text] -> Maybe a
  , iCompile :: a -> m ()
  }

data Invocation m a = Invocation
 { iRecognized :: [Text]
 , iParsed     :: Maybe a
 , iCompiled   :: m ()
-- , executed   :: Bool
 }

{-| the fields in the output are constructed lazily,
with intermediary computations shared.

-}
-- handleRequest :: CommandsHandlers a b -> CommandsRequest -> CommandsResponse a b
invoke :: (Applicative m) => Invoker m a -> [Text] -> Invocation m a
invoke Invoker{..} ws = Invocation{..}
 where
 iRecognized = ws
 iParsed     = iRecognized & iParse
 iCompiled   = iParsed & maybe (pure()) iCompile

{- |

@

$ stack build && stack exec -- commands-spiros-server
> camel two words
Right (Phrase_ (Phrase [Joined_ CamelJoiner,Dictated_ (Dictation ["two","words"])]))
...

@

'windowsHowToSendText' = 'defaultWindowsHowToSendText'
'windowsStepDelay'     = 'defaultWindowsStepDelay'

-}
mainWith :: _ -> IO ()
mainWith environment@VEnvironment{..} = do
  --	putStrLn $ displaySerializedGrammar (ePlugin&vGrammar)
  putStrLn "(commands-spiros-server...)"
  _ <- spirosSetup environment
  runSimpleServer _settings

  where
  _settings = (defaultSettings Windows.defaultWindowsExecuteWorkflow) -- TODO This is  platform specific / and currently ignored
    { handle = myHandle ePlugin
    , cmdln = Nothing -- Just $ myCmdln ePlugin
    }

{-| a command line (that is actively reading from standard input) may or may not make the app hang

-}
myCmdln ePlugin (words -> fmap T.pack -> ws) = do
  print $ bestParse (ePlugin&vParser) ws
  putStrLn""
  hFlush stdout

-- myHandle = defaultHandle
-- myHandle :: _ -> W.ExecuteWorkflow -> Recognition -> IO ()
myHandle :: _ -> W.ExecuteWorkflow -> [String] -> IO ()
myHandle ePlugin (W.ExecuteWorkflow _execute) (fmap T.pack -> recognition) =
-- execute_ $ printingDurationInMilliseconds "request" $ do
 printingDurationInMilliseconds "request" $ do
  -- TODO lol re- factor command-server-simple 
  let ws = recognition & cleanRecognition
  liftIO$ do
      putStrLn ""    
      putStrLn "----------------------------------------------------------------------------------"
      putStrLn$ "RECOGNITION:"
      print $ recognition

  -- context' <- Windows.currentApplication
  -- liftIO$ do
  --     putStrLn "----------------------------------------------------------------------------------"
  --     putStrLn$ "APP:"
  --     print $ context'
  --     putStrLn ""

  let context = GlobalContext -- ""

  liftIO$ case validateRecognition ws of
    Left (EncodingError invalidCharacters) -> do
      putStrLn$ "INVALID CHARACTERS:"
      putStrLn$ invalidCharacters
      putStrLn$ invalidCharacters & fmap ord &show
      putStrLn$ "WORDS:"
      putStrLn$ showWords ws
    Left (NoiseError) -> do -- TODO a "results type" for the parsing stage , either failure, success, or noise (and later maybe  other cases)
      putStrLn$ "NOISE:"
      print $ myNoise
      putStrLn$ "WORDS:"
      putStrLn$ showWords ws
    Right () -> do
      go context ws

  _ <- performGarbageCollection 
  putStrLn "\n(returned)"
  hFlush stdout

 where
 go context ws = do
  x' <- printingDurationInMilliseconds "parse" $ do
            return $ bestParse (ePlugin&vParser) ws 
  case x' of
    Right x -> do
      -- if (isNoise (ws & fmap T.unpack)) -- TODO a "results type" for the parsing stage , either failure, success, or noise (and later maybe  other cases)
        _display ws context x
        printingDurationInMilliseconds "execute" $ do
            exec context x

    Left e -> do
      liftIO$ do
           replicateM_ 3 (putStrLn"")
           putStrLn$ "ERROR:"
           print e
           putStrLn$ "WORDS:"
           putStrLn$ showWords ws
           hFlush stdout -- TODO why flush ?

 _display ws context value = liftIO$ do
      putStrLn ""
      putStrLn$ "VALUE:"
      print value
      putStrLn ""
      putStrLn$ "CONTEXT:"
      print context
      putStrLn ""
      putStrLn$ "WORDS:"
      putStrLn$ showWords ws
      -- putStrLn ""

 exec context value = liftIO$ do
    runSpirosMonad $ (ePlugin&vDesugar) context value
    -- runSpirosMonad $ (ePlugin&vDesugar) context value -- TODO

 printingDurationInMilliseconds s m = do 
   (x,t) <- measuringDuration m
   putStrLn "" 
   putStrLn $ "[action]:   " <> s 
   putStrLn $ "[duration]: " <> show t <> "ms"
   return x

 -- NOTE http://chrisdone.com/posts/measuring-duration-in-haskell
 -- 
 {- 
 Monotonic: a monotonic but not-absolute time which never changes after start-up.
 Realtime: an absolute Epoch-based time (which is the system clock and can change).
 ProcessCPUTime: CPU time taken by the process.
 ThreadCPUTime: CPU time taken by the thread. 
 -} 
 -- 
 -- https://ocharles.org.uk/blog/posts/2013-12-15-24-days-of-hackage-time.html
 -- 
 measuringDuration m = do 

   tBefore <- getNow 
   x <- m >>= evaluate -- forceEvaluation needs NFData
   tAfter <- getNow 
   let tDuration = diffTimeSpec tAfter tBefore 
   let t = millisecondsFromTimeSpec tDuration -- in milliseconds 
   return (x, t) -- pretty sure some of these are redundant or trivial, but I just want to make sure I'm actually measuring the computation 

   -- tBefore <- getCurrentTime 
   -- x <- m -- calculates the computation, doesn't necessarily evaluate the output 
   -- _ <- evaluate x -- evaluate the output too
   -- tAfter <- getCurrentTime -- TODO getTime Realtime  
   -- let tDuration = diffUTCTime tAfter tBefore 
   -- let t = millisecondsFromNominalDiffTime tDuration -- in milliseconds 
   -- return (x, t) -- pretty sure some of these are redundant or trivial, but I just want to make sure I'm actually measuring the computation 

 getNow :: IO TimeSpec
 getNow = getTime Realtime

 forceEvaluation :: (NFData a) => a -> IO a
 forceEvaluation x = evaluate (force x) 

 -- -- the unit of NominalDiffTime seems to be seconds 
 -- -- https://hackage.haskell.org/package/time-1.8.0.3/docs/Data-Time-Clock.html
 -- -- 
 -- millisecondsFromNominalDiffTime :: NominalDiffTime -> Natural -- TODO 
 -- millisecondsFromNominalDiffTime = (*1000) > ceiling 

 millisecondsFromTimeSpec :: TimeSpec -> Natural 
 millisecondsFromTimeSpec = toNanoSecs > (`div` 1000000) > fromIntegral
 
 performGarbageCollection = do 
    _ <- forkIO$ liftIO$ do
             delayMilliseconds 100 -- pause for a hundred milliseconds 
             -- TODO verify that the child thread is actually asynchronous and independent from the parent request thread 
             printingDurationInMilliseconds "collected garbage" $ do 
                 performMajorGC -- lol
    nothing
    -- TODO the garbage collection is stop-the-world, that includes other threads right? so we should wait a little, enough for the request to return 

--------------------------------------------------------------------------------

runSpirosMonad = getSpirosMonad > Windows.runWorkflowWithT def
  { Windows.windowsCharacterDelay=0
  , Windows.windowsDuplicateCharacterDelay = 40
  } --TODO

isNoise ws = (ws `elem` myNoise)
myNoise = fmap (:[]) $
  ["the","will","if","him","that","a","she","and","up","to","it","6"
  ] :: [MyRecognition]

type MyRecognition = [Text]

data ServerError
  = NoiseError
  | EncodingError [Char] -- the invalid characters

isAsciiRecognition :: MyRecognition -> Bool
isAsciiRecognition = all (T.all isAscii)

validateRecognition :: MyRecognition -> Either ServerError ()
validateRecognition recognition
 -- NOTE currently unnecessary, as Unicode insertion works on Windows 
 -- not (isAsciiRecognition recognition) = Left $ EncodingError (recognition & T.concat & T.filter (isAscii > not) & T.unpack)
 | isNoise recognition = Left $ NoiseError
 | otherwise = Right ()

{-|

>>> cleanRecognition ['spell', 'a\\\\spelling-letter\\\\A', ',\\\\comma\\\\comma', 'a\\\\determiner', 'letter']
["spell", "A", ",", "a", "letter"]

-}
cleanRecognition :: MyRecognition -> MyRecognition
cleanRecognition
  = fmap (T.splitOn "\\" > head) -- NOTE safely partial

----------------------------------------------------------------------------------

spirosUpdate
 :: DnsOptimizationSettings
 -- -> RULED ((Server.VSettings m c a)) r a
 -> (CMD c (m()) a)
 -> (VPlugin m c a)
spirosUpdate dnsSettings command = VPlugin g p d
 where
 g = (unsafeDNSGrammar dnsSettings (command&_cRHS))
 p = (EarleyParser (unsafeEarleyProd (command&_cRHS)) (command&_cBest))
 d = (command&_cDesugar)
{-# NOINLINE spirosUpdate #-}

type CMD c = DNSEarleyCommand c --TODO

{- | read-only.

"dynamic" configuration

-}
data VPlugin m c v = VPlugin
 { vGrammar :: DNS.SerializedGrammar
 , vParser  :: (forall s r. EarleyParser s r String Text v)
 , vDesugar :: c -> v -> m ()
 }

defaultHandle = handle
 where
 handle ws = if (_ignore ws) then nothing else Windows.sendText (_munge ws)
 _munge = unwords > fmap toLower > (++ " ")
 _ignore = unwords > (`elem` _noise)
 _noise = ["the","will","if","him","that","a","she","and"]

--
--TODO
--command2earley :: CMD c b a -> EarleyParser s r String Text a
--command2earley Command{..} = EarleyParser (unsafeEarleyProd _cRHS) _cBest

printMessage :: [String] -> IO ()
printMessage = putStrLn . List.intercalate "\n"

--------------------------------------------------------------------------------

data VEnvironment m c v = VEnvironment
 { ePlugin :: VPlugin m c v
 , eSettings :: NatLinkSettings
 }

spirosSetup
 :: _
 -> IO (Either String ())
-- spirosSetup VEnvironment{ePlugin{..},eConfig{..}} = do
spirosSetup environment = do
 let theGrammar = (environment&ePlugin&vGrammar)
 let NatLinkSettings{..} = environment&eSettings -- TODO theAddress
 -- let theSettings@NatLinkSettings{..} = environment&eSettings -- TODO theAddress
 let theConfig = DNS.NatLinkConfig nlAddress narcissisticGrammarProperties -- TODO LOL get rid of  config

 do
   putStrLn ""
   T.putStrLn$ displayAddress nlAddress
   -- T.putStrLn$ curl_ExampleRequest nlAddress

 -- do
 --   let myBatchScript = getBatchScript myBatchScriptR
 --
 --   putStrLn ""
 --   T.putStrLn$ myBatchScript
 --   setClipboardIO$ T.unpack myBatchScript
 --
 --   putStrLn ""
 --   putStrLn$ getBatchScriptPath myBatchScriptR
 --
 --   setClipboardIO$ T.unpack (myBatchScriptR&__batchFilePath__)

 let theShim = fmap (over _PythonFile _cleanShim) $ (DNS.applyShim DNS.getShim theConfig theGrammar)  -- TODO is this the right place?

 -- TODO https://hackage.haskell.org/package/temporary-1.2.1.1/docs/System-IO-Temp.html
 case theShim of

  Left (PythonSyntaxError e s) -> do
   let (errorRow, errorColumn) = getPythonErrorSpan e
   let (_marginWidth, _countWidth, code) = leftAppendLineNumbers s

   putStrLn ""
   print e
   putStrLn ""
   T.putStrLn$ code
   putStrLn ""
   -- Windows.runWorkflowT def $ findErrorBySearch countWidth marginWidth errorRow errorColumn
   putStrLn $ (show errorRow)<>":"<> (show errorColumn)
   putStrLn "SHIM PARSING FAILURE" -- TODO logging
   return$ Left("")

  Right myShim -> do

   T.putStrLn $ "writing shim to '"<>T.pack nlLocation<>"'"
   T.writeFile nlLocation (myShim&getPythonFile) --- Todo?

   -- putStrLn$ T.unpack shim  -- too long (5k lines)
   putStrLn ""

  --  -- because:
  --  -- 1. when pasting into an editor in virtual box, the clipboard contents are often trailed by Unicode garbage
  --  -- 2. which is why the shim ends in a comment
  --  -- 3. but Unicode characters can have nonlocal effects on other characters, like on previous lines
  --  copyShim myShim
  --  writeShim myBatchScriptR myShim -- TODO its own function

   putStrLn "SHIM PARSING SUCCESS" -- TODO logging

   return$ Right()

  where
  _cleanShim :: Text -> Text
  _cleanShim = T.filter isAscii
