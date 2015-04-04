{-# LANGUAGE FlexibleContexts, RankNTypes, TemplateHaskell #-}
module Commands.Backends.OSX.DSL where
import Commands.Backends.OSX.Types

import Control.Monad.Free
import Control.Monad.Free.TH       (makeFree)

import Data.Foldable               (traverse_)
import Data.Monoid                 ((<>))


makeFree ''OSXAction

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

insert :: String -> Actions ()
insert
 = traverse_ (uncurry sendKeyPress)
 . concatMap touch

{- | \"touch" the right key for the char.

>>> touch '@' :: Maybe KeyPress
Just (KeyPress [Shift] TwoKey)

>>> touch '\0' :: Maybe KeyPress
Nothing


-}
touch :: Char -> [((,) [Modifier] Key)]

touch 'a'  = return $ (,) [     ] AKey
touch 'A'  = return $ (,) [Shift] AKey
touch 'b'  = return $ (,) [     ] BKey
touch 'B'  = return $ (,) [Shift] BKey
touch 'c'  = return $ (,) [     ] CKey
touch 'C'  = return $ (,) [Shift] CKey
touch 'd'  = return $ (,) [     ] DKey
touch 'D'  = return $ (,) [Shift] DKey
touch 'e'  = return $ (,) [     ] EKey
touch 'E'  = return $ (,) [Shift] EKey
touch 'f'  = return $ (,) [     ] FKey
touch 'F'  = return $ (,) [Shift] FKey
touch 'g'  = return $ (,) [     ] GKey
touch 'G'  = return $ (,) [Shift] GKey
touch 'h'  = return $ (,) [     ] HKey
touch 'H'  = return $ (,) [Shift] HKey
touch 'i'  = return $ (,) [     ] IKey
touch 'I'  = return $ (,) [Shift] IKey
touch 'j'  = return $ (,) [     ] JKey
touch 'J'  = return $ (,) [Shift] JKey
touch 'k'  = return $ (,) [     ] KKey
touch 'K'  = return $ (,) [Shift] KKey
touch 'l'  = return $ (,) [     ] LKey
touch 'L'  = return $ (,) [Shift] LKey
touch 'm'  = return $ (,) [     ] MKey
touch 'M'  = return $ (,) [Shift] MKey
touch 'n'  = return $ (,) [     ] NKey
touch 'N'  = return $ (,) [Shift] NKey
touch 'o'  = return $ (,) [     ] OKey
touch 'O'  = return $ (,) [Shift] OKey
touch 'p'  = return $ (,) [     ] PKey
touch 'P'  = return $ (,) [Shift] PKey
touch 'q'  = return $ (,) [     ] QKey
touch 'Q'  = return $ (,) [Shift] QKey
touch 'r'  = return $ (,) [     ] RKey
touch 'R'  = return $ (,) [Shift] RKey
touch 's'  = return $ (,) [     ] SKey
touch 'S'  = return $ (,) [Shift] SKey
touch 't'  = return $ (,) [     ] TKey
touch 'T'  = return $ (,) [Shift] TKey
touch 'u'  = return $ (,) [     ] UKey
touch 'U'  = return $ (,) [Shift] UKey
touch 'v'  = return $ (,) [     ] VKey
touch 'V'  = return $ (,) [Shift] VKey
touch 'w'  = return $ (,) [     ] WKey
touch 'W'  = return $ (,) [Shift] WKey
touch 'x'  = return $ (,) [     ] XKey
touch 'X'  = return $ (,) [Shift] XKey
touch 'y'  = return $ (,) [     ] YKey
touch 'Y'  = return $ (,) [Shift] YKey
touch 'z'  = return $ (,) [     ] ZKey
touch 'Z'  = return $ (,) [Shift] ZKey

touch '0'  = return $ (,) [     ] ZeroKey
touch ')'  = return $ (,) [Shift] ZeroKey
touch '1'  = return $ (,) [     ] OneKey
touch '!'  = return $ (,) [Shift] OneKey
touch '2'  = return $ (,) [     ] TwoKey
touch '@'  = return $ (,) [Shift] TwoKey
touch '3'  = return $ (,) [     ] ThreeKey
touch '#'  = return $ (,) [Shift] ThreeKey
touch '4'  = return $ (,) [     ] FourKey
touch '$'  = return $ (,) [Shift] FourKey
touch '5'  = return $ (,) [     ] FiveKey
touch '%'  = return $ (,) [Shift] FiveKey
touch '6'  = return $ (,) [     ] SixKey
touch '^'  = return $ (,) [Shift] SixKey
touch '7'  = return $ (,) [     ] SevenKey
touch '&'  = return $ (,) [Shift] SevenKey
touch '8'  = return $ (,) [     ] EightKey
touch '*'  = return $ (,) [Shift] EightKey
touch '9'  = return $ (,) [     ] NineKey
touch '('  = return $ (,) [Shift] NineKey

touch '`'  = return $ (,) [     ] GraveKey
touch '~'  = return $ (,) [Shift] GraveKey
touch '-'  = return $ (,) [     ] MinusKey
touch '_'  = return $ (,) [Shift] MinusKey
touch '='  = return $ (,) [     ] EqualKey
touch '+'  = return $ (,) [Shift] EqualKey
touch '['  = return $ (,) [     ] LeftBracketKey
touch '{'  = return $ (,) [Shift] LeftBracketKey
touch ']'  = return $ (,) [     ] RightBracketKey
touch '}'  = return $ (,) [Shift] RightBracketKey
touch '\\' = return $ (,) [     ] BackslashKey
touch '|'  = return $ (,) [Shift] BackslashKey
touch ';'  = return $ (,) [     ] SemicolonKey
touch ':'  = return $ (,) [Shift] SemicolonKey
touch '\'' = return $ (,) [     ] QuoteKey
touch '"'  = return $ (,) [Shift] QuoteKey
touch ','  = return $ (,) [     ] CommaKey
touch '<'  = return $ (,) [Shift] CommaKey
touch '.'  = return $ (,) [     ] PeriodKey
touch '>'  = return $ (,) [Shift] PeriodKey
touch '/'  = return $ (,) [     ] SlashKey
touch '?'  = return $ (,) [Shift] SlashKey

touch ' '  = return $ (,) [     ] SpaceKey
touch '\t' = return $ (,) [     ] TabKey
touch '\n' = return $ (,) [     ] ReturnKey

touch c    = fail $ "Commands.Compile.touch: un-pressable character: " <> show c
