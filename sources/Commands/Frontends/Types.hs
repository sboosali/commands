{-| Interface for speech recognition engines.

Naming: they are the "frontends" to a @commands@ server, producing recognitions.

The dozen most-used speech recognition engines are completely different from each other. Differences include:

* @Customization@: Can a user provide a custom context-free grammar, to bias recognition? Or at least a vocabulary of phrases?
* @Dictation@: Can arbitrary sentences (any words, any length) be recognized (see "large vocabulary continuous speech recognition")? Or only a (fixed/finite) vocabulary of phrases?
* @Session@: Can the user dynamically updated the speech recognition "session"? In particular, can the grammar be updated dynamically, if @Customization@ is supported? What resources must be acquired/released? Is there an actual bidirectional channel (like @websockets@), or the communication ad-hoc? (obviously it's always "unidirectional" at the least, producing recognitions)

Compare:

* @Dragon NaturallySpeaking (DNS)@: yes @Customization@ (any CFG), yes @Dictation@
* @Windows Speech Recognition (WSR)@: yes @Customization@ (any CFG), yes @Dictation@
* @Google Speech API@: no Customization, yes Dictation
* @NSSpeechRecognizer@: yes @Customization@ (vocabulary only), no @Dictation@
* @pocketsphinx@: yes @Customization@ (vocabulary only), no @Dictation@

Meaning, there's not much to "standardize".
However, the interface is expressive enough to support the following implementations:

* @commands-frontend-dragon13@ <>
* @commands-frontend-NSSpeechRecognizer@ <>

and convenient enough to be coded against in the following "commands servers":

* @commands-server-simple@ <>
* @commands-server-featured@ <>

-}
module Commands.Frontends.Types where

import qualified Pipes.Core as P


{-|

-}
data Frontend = Frontend
 { fRecognitions :: P.Producer String IO ()
 , fCreate  :: ()
 , fDestroy :: ()
 }


{-TODO


 fRecognitions :: P.Server request=(Maybe Recognizer) response=Recognition m ()

i.e.

Handle the response (Server action, in m)
Update the recognizer state (on client), if different.

type Server request response m a = Proxy ...?

type Recognizing = Server (Maybe Recognizer) Recognition IO ()
fRecognitions : Recognizing

data Recognizer
data Recognition

e.g.
Recognizer  = [[String]] -- The vocabulary of sentences that can be recognized
Recognition = [String]   -- The sentence recognized


class? can just data work?

data Frontend recognizing recognition =
 { fRecognitions :: Server request response IO ()
 }

e.g.
type DragonNaturallySpeakingFrontend = Frontend DNSState [DNSToken]

Since data is first class, While typeclasses are not, We can do things like Transforming Frontends:

e.g.
: Frontend DNSState [DNSToken] -> Frontend DNSState [String]
-- Strip the Pronunciation/formatting/word-sense metadata
Frontend r1 = Frontend r2 where r2 = P.map (\(DNSToken s _ _ _) -> s) r1

The 'respond' (and Recognizer update) Can be implemented In different ways itself:
(Current implementation) Via HTTP, Even within the virtual machine, With Hey running Natlink(Python) client
(Not implemented) With shared memory, If on Windows machines, Linking against some Haskell Dragon plug-in


-}

{-old

-- type Recognition = [String]
--TODO Recognition depends on the frontend: can be [String], or structured like DNS's triplets.

@
type 'P.Server' request response m a = Proxy ...
@
-- being client-driven seems to be the most flexible channel to get data from the speech engine.

 { fRecognitions :: P.Server (Maybe recognizer) recognition IO ()


-}
