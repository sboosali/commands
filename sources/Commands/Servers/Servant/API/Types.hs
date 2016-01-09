{-# LANGUAGE DataKinds, TypeOperators, KindSignatures #-}
module Commands.Servers.Servant.API.Types where
import Commands.Servers.Servant.Types 

import Servant 

import GHC.TypeLits (Symbol) 


{-| signature for a simple foreign function, via JSON and HTTP. 

-}
type PostAPI (s :: Symbol) (a :: *) (b :: *)
 =  s
 :> ReqBody '[JSON] a
 :> Post    '[JSON] b 

{-| the complete API between a @command server@ and a @natlink client@. 

-}
type NatlinkAPI = RecognitionAPI :<|> HypothesesAPI :<|> CorrectionAPI :<|> ReloadAPI :<|> ContextAPI

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
type RecognitionAPI = PostAPI "recognition" RecognitionRequest DNSResponse

{-| the API for correcting recognition, given Dragon's hypotheses. 

* derive a Haskell __client__ and a UI __server__. 
* derive a Haskell __server__ (that forwards to the UI server) 

e.g. UI server: Emacs can embed a server and has a good UI 

e.g. trace:

@
frontend  --hypothesis-->  commands  --hypothesis(forwarded)-->  ui  --correction-->  commands  
... 
frontend  --(anything)-->  commands  --update(+correction)-->  frontend 
@

the @...@ means that we must wait until the next frontend request, 
as we assume the front-end can't embed a server. e.g. the Dragon NaturallySpeaking front-end (Natlink) 
is single-threaded and callback-driven. thus, most of our http responses are @()@, 
and we pack their responses into a single @Update@.  

-}
type HypothesesAPI       = PostAPI "hypotheses" HypothesesRequest DNSResponse
type HypothesesClientAPI = PostAPI "hypotheses" HypothesesRequest () 

{-| the API for TODO 

-}
type CorrectionAPI = PostAPI "correction" CorrectionRequest DNSResponse

{-| the API for TODO 

-}
type ReloadAPI = PostAPI "reload" ReloadRequest DNSResponse


{-| the API for TODO 

-}
type ContextAPI = PostAPI "context" ContextRequest DNSResponse


-- ================================================================ --

natlinkAPI :: Proxy NatlinkAPI
natlinkAPI = Proxy

recognitionAPI :: Proxy RecognitionAPI
recognitionAPI = Proxy 

hypothesesAPI :: Proxy HypothesesAPI
hypothesesAPI = Proxy 

hypothesesClientAPI :: Proxy HypothesesClientAPI
hypothesesClientAPI = Proxy 

correctionAPI :: Proxy CorrectionAPI
correctionAPI = Proxy 

reloadAPI :: Proxy ReloadAPI
reloadAPI = Proxy 

contextAPI :: Proxy ContextAPI
contextAPI = Proxy 

