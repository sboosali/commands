{-# LANGUAGE DataKinds  #-}
module Commands.Command.Types where
import Commands.Grammar.Types
import Commands.Parse.Types
import Commands.Frontends.Dragon13.Types
import Data.Text.Lazy (Text)


data Command a = Command (Rule a) (DNSProduction True Text Text) (Parser a)

