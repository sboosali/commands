{-# LANGUAGE RecursiveDo, PatternSynonyms, FlexibleContexts                                 #-}
module Commands.Sugar.Keys where  
import           Commands.Extra
import           Commands.Backends.OSX
import           Commands.Mixins.DNS13OSX9.Types  (EarleyEither,toEarleyEither)

import qualified Data.List.NonEmpty              as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import qualified Text.Earley                     as E

import           Control.Applicative
import           Data.Char hiding(Control)


{- | parse a string, execute it as a keyboard shortcut 

@press = 'runKeyRiff' . 'kbd'@

warning: partial function 
-}
press :: (MonadWorkflow m) => String -> m () 
press = runKeyRiff . kbd

-- | execute a keyboard shortcut 
runKeyRiff :: (MonadWorkflow m) => KeyRiff -> m () 
runKeyRiff = traverse_ (\(KeyChord mods k) -> sendKeyChord mods k)

{- | a grammar for Emacs-like keybindings syntax, like elisp @kbd@ (<http://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Sequences.html>).

>>> kbd"a"
[([],AKey)]

>>> kbd"C-M-b"
[([Control,CommandMod],BKey)]

>>> kbd"M-S-a"
[([CommandMod, Shift],AKey)]

>>> kbd"C-<tab>"
[([CommandMod],TabKey)]

>>> kbd"A-\\"
[([Option],BackslashKey)]

>>> kbd"C-x o C-x b"
[([Control],XKey),([],OKey),([Control],XKey),([],BKey)]

warning: partial function.

-}
kbd :: [Char] -> KeyRiff
kbd cs = either (error.(("(Commands.Sugar.Keys.kbd "++(show cs)++")") ++).show) NonEmpty.head (safeKbd cs)

-- | like @kbd@, but a total function 
safeKbd :: [Char] -> EarleyEither String Char (NonEmpty KeyRiff)
safeKbd cs = toEarleyEither $ E.fullParses (E.parser gKeychords (strip cs))
-- bimapEither toException .
 where
 strip :: String -> String
 strip = rstrip . lstrip
 lstrip = dropWhile (`elem` (" \t\n\r"::String))
 rstrip = reverse . lstrip . reverse

{- | a grammar for Emacs-like keybindings syntax.

follows (a subset of) <http://emacswiki.org/emacs/EmacsKeyNotation Emacs keybinding syntax>. some differences: 

* doesn't interpret uppercase non-modifier letters as shifted alphanumerics, for disambiguation. 
* e.g. use @"M-S-a"@, not @"M-A"@. non-alphanumerics are in angle brackets. 
* e.g. @"C-<tab>"@, not @"C-TAB"@. see the source (the parser combinators are readable) for other differences.

-}
gKeychords :: E.Grammar r String (E.Prod r String Char KeyRiff)
gKeychords = mdo

 pKeychords <- E.rule$ (:) <$> pKeychord <*> many (some (E.symbol ' ') *> pKeychord)
               E.<?> "keychords"

 pKeychord  <- E.rule$ toKeychord <$> many (pModifier <* E.symbol '-') <*> pKey
               E.<?> "keychord"

 pModifier  <- E.rule$ empty
  <|> E.symbol 'M' $> CommandModifier  -- generally, the meta-key
  <|> E.symbol 'C' $> ControlModifier
  <|> E.symbol 'S' $> ShiftModifier
  <|> E.symbol 'A' $> OptionModifier
  E.<?> "modifier"

 pKey       <- E.rule$ empty

  <|> NoMod SpaceKey                             <$  E.word "<spc>"
  <|> NoMod TabKey                               <$  E.word "<tab>"
  <|> NoMod ReturnKey                            <$  E.word "<ret>"
  <|> NoMod DeleteKey                            <$  E.word "<del>"
  <|> NoMod EscapeKey                            <$  E.word "<esc>"

  <|> NoMod UpArrowKey     <$ E.word  "<up>"
  <|> NoMod DownArrowKey   <$ E.word  "<down>" 
  <|> NoMod LeftArrowKey   <$ E.word  "<left>" 
  <|> NoMod RightArrowKey  <$ E.word  "<right>" 

  <|> NoMod F1Key <$ E.word  "<f1>"
  <|> NoMod F2Key <$ E.word  "<f2>"
  <|> NoMod F3Key <$ E.word  "<f3>"
  <|> NoMod F4Key <$ E.word  "<f4>"
  <|> NoMod F5Key <$ E.word  "<f5>"
  <|> NoMod F6Key <$ E.word  "<f6>"
  <|> NoMod F7Key <$ E.word  "<f7>"
  <|> NoMod F8Key <$ E.word  "<f8>"
  <|> NoMod F9Key <$ E.word  "<f9>"
  <|> NoMod F10Key <$ E.word  "<f10>"
  <|> NoMod F11Key <$ E.word  "<f11>"
  <|> NoMod F12Key <$ E.word  "<f12>"
  <|> NoMod F13Key <$ E.word  "<f13>"
  <|> NoMod F14Key <$ E.word  "<f14>"
  <|> NoMod F15Key <$ E.word  "<f15>"
  <|> NoMod F16Key <$ E.word  "<f16>"
  <|> NoMod F17Key <$ E.word  "<f17>"
  <|> NoMod F18Key <$ E.word  "<f18>"
  <|> NoMod F19Key <$ E.word  "<f19>"
  <|> NoMod F20Key <$ E.word  "<f20>"

  <|> (either __BUG__ id . digit2keypress . read . (:[])) <$> E.satisfy ((&&) <$> isDigit <*> isAscii)
      -- read and digit2keypress are safe because: isDigit returns number characters between zero and nine inclusive
  <|> (either __BUG__ id . char2keypress) <$> E.satisfy ((&&) <$> (not.isAlphaNum) <*> isAscii)
      -- must be last 
  <|> (either __BUG__ id . char2keypress) <$> E.satisfy ((&&) <$> isLower <*> isAscii) -- TODO replace pure bug with effectfull empty 
      -- must be last 

  E.<?> "key"

 return pKeychords

 where
 toKeychord = lAppendModifiers
 lAppendModifiers ms (ms', k) = KeyChord (ms ++ ms') k

-- TODO a numerate all keybindings with one modifier and one non-modifier key, it's for documentation

