{-# LANGUAGE TemplateHaskell, DeriveAnyClass #-}
{-# LANGUAGE DataKinds, TypeOperators, KindSignatures #-}
{-|

-}
module Commands.Servers.Simple.Types where

import qualified Workflow.Core as W

import Servant
import           Data.Text.Lazy                        (Text)
import           Control.Monad.Trans.Except            (ExceptT)

import GHC.TypeLits (Symbol)
import Data.Char(toLower)
import Control.Monad (unless)

import Prelude.Spiros
import Prelude()

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

{-| Platform-independent settings for the server.

-}
data Settings = Settings
 { handle               :: [String] -> W.WorkflowT IO () -- tokenized
 , exec                 :: W.ExecuteWorkflow
 , cmdln                :: Maybe (String -> IO ()) -- not tokenized
 , port                 :: Int
 }

defaultSettings :: W.ExecuteWorkflow -> Settings
defaultSettings exec = Settings{..}
 where
 handle ws = unless (ignore ws) $ W.sendText (munge ws)
 munge = unwords > fmap toLower > (++ " ")
 ignore = unwords > (`elem` noise)
 noise = ["the","will","if","him","that","a","she","and"]
 cmdln = Just $ \s -> putStrLn (munge (words s)) -- echoes
 port  = 8888

recognitionAPI :: Proxy RecognitionAPI
recognitionAPI = Proxy
