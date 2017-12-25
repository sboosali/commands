{-# LANGUAGE DeriveAnyClass #-}
module Commands.Plugins.Spiros.Shell.Types where 
import           Commands.Plugins.Spiros.Extra.Types 
import           Commands.Plugins.Spiros.Phrase.Types 


data Safety
 = Safe     -- ^ shell commands that have no side effects, e.g. "ls"
            -- (it writes to the history, but that's a benign side effect). 
            --   or can be easily undone, e.g. "git stash". 
 | Unsafe   -- ^ e.g. "rm"  
 deriving (Show,Read,Eq,Ord,Bounded,Enum,Generic,Data,NFData)

data Shell                     
 = Shell Safety String Phrase -- TODO "safety" depends on options/subcommands, not just the command itself 
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData)
