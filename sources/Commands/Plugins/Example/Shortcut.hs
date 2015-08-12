{-# LANGUAGE  TemplateHaskell, OverloadedStrings, PostfixOperators, RankNTypes, LambdaCase       #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Example.Shortcut where
import Commands.Etc
import Commands.Mixins.DNS13OSX9
import Commands.Plugins.Example.Press

import           Data.Text.Lazy                 (Text)

import           GHC.Exts                        (IsString (..))
import Control.Applicative


shortcuts :: (Functor'RHS n Text f) => [(String,String)] -> RHS n Text f KeyRiff
shortcuts = foldMap $ \case
 ("","") -> empty               -- for convenience
 (s, k) -> keys k <$ fromString s

-- TODO global context (e.g. all Apps) should be overridden by a local context (e.g. some App)
myShortcuts = 'myShortcuts <=> shortcuts
 -- <|> "copy" $> keys"M-c"
 -- <|> "copy" $> [KeyPress [CommandMod] CKey]
 [ "undo"-: "M-z"
 , "salt"-: "M-a"
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 ]
