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

Meaning, there's not much to "unify". However, the interface is expressive enough to support the following implementations:

* @commands-frontend-dragon13@ <>
* @commands-frontend-NSSpeechRecognizer@ <>

and convenient enough to be coded against in the following "commands servers":

* @commands-server-simple@ <>
* @commands-server-featured@ <>

-}
module Commands.Frontends.Types where

import qualified Pipes as P

data Frontend = Frontend
 { fRecognitions :: P.Producer Recognition IO ()
 }

type Recognition = [String]
--TODO Recognition depends on the frontend: can be [String], or structured like DNS's triplets.

-- being client-driven seems to be the most flexible channel to get data from the speech engine.
