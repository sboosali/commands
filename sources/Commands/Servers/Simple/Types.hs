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

import Prelude.Spiros
import Prelude()

type RecognitionHandler = Recognition -> Response ()

{-| The server's response.

-}
type Response = ExceptT ServantErr IO

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

@
$ export PORT=8666
$ curl  -X POST  -H "Content-Type: application/json"  -d '["some","words"]'  "http://localhost:$PORT/recognition/"
$ python -c 'import sys,os,json,urllib2; print (urllib2.urlopen(urllib2.Request("http://localhost:"+os.environ["PORT"]+"/recognition/", json.dumps(["some","words with spaces"]), {"Content-Type": "application/json"})).readline())'
@

-}
type RecognitionAPI = PostAPI "recognition" Recognition ()

{- | a successful recognition of an utterance.

>>> 'decode' "[\"hello\",\"world\"]" :: Maybe Recognition
Just ["hello","world"]

-}
type Recognition = [String]

{-| Platform-independent settings for the server.

-}
data Settings = Settings
 { munge                :: [String] -> String
 , ignore               :: [String] -> Bool
 , exec                 :: W.ExecuteWorkflow
 , port                 :: Int
 }

defaultSettings :: W.ExecuteWorkflow -> Settings
defaultSettings exec = Settings{..}
 where
 munge = unwords > fmap toLower > (++ " ")
 ignore = unwords > (`elem` noise)
 port  = 8888
 noise = ["the","will","if","him","that","a","she","and"]

recognitionAPI :: Proxy RecognitionAPI
recognitionAPI = Proxy
