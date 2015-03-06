{-# LANGUAGE DeriveFunctor #-}
module Commands.Command.Types where
import Commands.Etc
import Commands.Frontends.Dragon13.Types
-- import Commands.Frontends.Dragon13.Text
import Commands.Parse.Types




-- |
--
-- Command ~ LHS * DNSGrammar Text Text * Parser a
--
-- RHS ~ Alt Symbol ~ Constant Word + Command ~ Constant Word + (LHS * DNSGrammar Text Text * Parser a)
--
data Command a = Command
 { _lhs     :: GUI
 -- , _rule     :: Rule a
 , _grammar :: (DNSGrammar String String)
 , _parser  :: (Parser a)
 }
 deriving (Functor)

