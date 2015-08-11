{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes #-}
{-# LANGUAGE TemplateHaskell, ViewPatterns                   #-}
module Commands.Backends.OSX.DSL where
import           Commands.Backends.OSX.Types

import           Control.Monad.Free          (MonadFree, liftF)
import           Control.Monad.Free.TH       (makeFree)
import qualified Data.ByteString.Char8       as BS
import           Network.HTTP.Types.URI      (renderQuery)

import           Data.Monoid                 ((<>))


makeFree ''ActionF

insert :: String -> Actions ()
insert = sendText

-- TODO
-- wait (25 ms)
-- wait $ 25 ms
-- wait (25ms)
--
-- ms :: TimeUnit
-- instance Num (TimeUnit -> Time)
-- instance Num (UnitOf a -> a)
--
-- module Commands.Compiler.Sugar where

-- I want "type = mapM_ (press . KeyPress [] . key)" to be "atomic" i.e. no delay between each step. I want "press (KeyPress [Command] RKey) >> type text" to be "laggy" i.e. some delay between each step. If I "instrument" some "Actions", by interleaving "Wait 25" between each step i.e. each "Free _", I can't distinguish between groups of steps. Thus, I should manually insert "Wait 25" between any steps that need some lag, or "automate it locally" in helper functions, but not "automate it globally" by interleaving.

copy :: Actions String
copy = do
 sendKeyPress [CommandMod] CKey
 delay 25 -- TODO does it need to wait? how long? delay $ milliseconds 25
 getClipboard

paste :: Actions ()
paste = do
 sendKeyPress [CommandMod] VKey

google :: String -> Actions ()
google (BS.pack -> query) = openURL (BS.unpack $ "https://www.google.com/search" <> renderQuery True [("q", Just query)])

-- TODO
-- instance convert KeyPress Actions
-- instance convert MouseClick Actions
