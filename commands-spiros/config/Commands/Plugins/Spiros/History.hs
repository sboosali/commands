{-# LANGUAGE NoImplicitPrelude, TemplateHaskellQuotes, OverloadedStrings, PostfixOperators, NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.History  where
import Commands.Plugins.Spiros.Extra
import Commands.Mixins.DNS13OSX9
import Control.Applicative
import Prelude.Spiros

data Nullify = Nullify
 deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

nullify = asum
  [ Nullify <$ "nullify"
  , empty
  ]

runNullify :: (MonadWorkflow m, MonadCapability Textual m) => Executor m Nullify
runNullify = \case
  Nullify -> do
    cTextual <- getCapabilities ^. pTextual
    r <- getPreviousRecognition
    (cTextual&toText) (r^.textualCommand) & maybe nothing nullifyInsertion
  where
  nullifyInsertion s = replicateM_ (length s) delete
  delete = pressKey DeleteKey
