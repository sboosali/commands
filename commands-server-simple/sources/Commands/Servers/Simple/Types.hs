{-# LANGUAGE TemplateHaskell, DeriveAnyClass #-}
{-# LANGUAGE DataKinds, TypeOperators, KindSignatures #-}
{-|

-}
module Commands.Servers.Simple.Types where

import qualified Workflow.Core as W

import Servant
-- import           Data.Text.Lazy                        (Text)
-- import           Control.Monad.Trans.Except            (ExceptT)

import GHC.TypeLits (Symbol)
import Data.Char(toLower)
-- import Control.Monad (unless)
import qualified Data.Map as Map

import Prelude.Spiros hiding (Handler)

type RecognitionHandler = Recognition -> Response ()

{-| The server's response.

-}
type Response = Handler -- ExceptT ServantErr IO

{-| signature for a simple foreign function, via JSON and HTTP.

-}
type PostAPI (s :: Symbol) (a :: *) (b :: *)
 =  s
 :> ReqBody '[JSON] a
 :> Post    '[JSON] b

{- | the API for a successful recognition.

@
POST /recognition
@

e.g.

Testing on UNIX:

@
$ export PORT=8888
$ curl  -X POST  -H "Content-Type: application/json"  -d '["some","words"]'  "http://localhost:$PORT/recognition/"
$ python -c 'import sys,os,json,urllib2; print (urllib2.urlopen(urllib2.Request("http://localhost:"+os.environ["PORT"]+"/recognition/", json.dumps(["some","words with spaces"]), {"Content-Type": "application/json"})).readline())'
@

Testing on Windows:

@
> set PORT=8888
> python -c "import sys,os,json,urllib2; print (urllib2.urlopen(urllib2.Request('http://localhost:'+os.environ['PORT']+'/recognition/', json.dumps(['some','words with spaces']), {'Content-Type': 'application/json'})).readline())"
@

Or run a simple test from the browser, by visiting
(you must change the port if it's not default) the following:

@
http://localhost:8888/test

or

http://127.0.0.1:8888/test
@

-}
type RecognitionAPI
     = PostAPI "recognition" Recognition ()
  :<|> "test" :> Get '[JSON] String

{- | a successful recognition of an utterance.

>>> 'decode' "[\"hello\",\"world\"]" :: Maybe Recognition
Just ["hello","world"]

-}
type Recognition = [String]
-- newtype Recognition = Recognition [String]

{-| Platform-independent settings for the server.

-}
data Settings = Settings
-- { handle               :: Recognition -> IO ()
 { handle :: W.ExecuteWorkflow -> Recognition -> IO ()
 -- W.WorkflowT IO () -- tokenized
 , exec                 :: W.ExecuteWorkflow
 , cmdln                :: Maybe (String -> IO ()) -- not tokenized
 , port                 :: Int
 }

data Action_ = Ignored_ | Shortcut_ String | Dictated_ String deriving (Show)

defaultSettings :: W.ExecuteWorkflow -> Settings
defaultSettings exec = Settings{..}
 where
 -- handle = (exec&W.getExecuteWorkflow) defaultHandler
 -- handle = defaultHandler exec
 handle = defaultHandler   
 cmdln = Just defaultCmdln
 port  = 8888

-- defaultHandler :: Recognition -> IO ()
-- defaultHandler ws = do
defaultHandler :: W.ExecuteWorkflow -> Recognition -> IO ()
defaultHandler (W.ExecuteWorkflow _execute) ws = _execute $ do
 liftIO$ putStrLn ""
 liftIO$ print ws
 liftIO$ print a
 
 case a of
   Shortcut_ kbd -> W.press kbd
   Dictated_ vs  -> W.sendText vs
   Ignored_      -> nothing
   
 where
 a = defaultParseAction ws

defaultParseAction :: Recognition -> Action_
defaultParseAction = go
 where
 go ws
  | isNoise ws = Ignored_
  | otherwise  = isShortcut vs & maybe (Dictated_ (vs  ++ " ")) Shortcut_ 
   where
   vs = munge ws
 isNoise = munge > (`elem` noise)
 munge = unwords > fmap toLower
 noise = ["the","will","if","him","that","a","she","and"]
 isShortcut = (Map.lookup&flip) shortcuts
 shortcuts = Map.fromList
  [ "copy"-: "C-c"
  , "paste"-: "C-v"
  , "undo"-: "C-z"
  ]

defaultCmdln :: String -> IO ()
defaultCmdln s = do
  putStrLn (munge s) -- echoes
  where
  munge = fmap toLower

recognitionAPI :: Proxy RecognitionAPI
recognitionAPI = Proxy
