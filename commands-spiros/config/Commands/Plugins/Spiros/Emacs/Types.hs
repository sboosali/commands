{-# LANGUAGE DeriveAnyClass #-} 
module Commands.Plugins.Spiros.Emacs.Types where
import           Commands.Plugins.Spiros.Extra.Types 
import           Commands.Plugins.Spiros.Phrase.Types 


type ElispSexp = String
-- -- type ElispSexp = Sexp String String
data Emacs
 = EmacsFunction (Maybe Phrase)
 | EmacsExpression (Maybe Phrase)
 -- TODO | EmacsKeyriff Keyriff
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData)

