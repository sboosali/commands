{-# LANGUAGE FlexibleContexts, RankNTypes, TemplateHaskell #-}
module Commands.Backends.OSX.DSL where
import Commands.Backends.OSX.Types

import Control.Monad.Free          (MonadFree, liftF)
import Control.Monad.Free.TH       (makeFree)


makeFree ''ActionF

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
 sendKeyPress [Command] CKey
 -- TODO does it need to wait? wait $ milliseconds 25
 getClipboard

paste :: Actions ()
paste = do
 sendKeyPress [Command] VKey

-- TODO
-- instance convert KeyPress Actions
-- instance convert MouseClick Actions

-- TODO
-- google :: String -> URL
-- google query = [qq| https://www.google.com/search?q={query} |]

