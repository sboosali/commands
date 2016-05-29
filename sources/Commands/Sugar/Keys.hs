{-# LANGUAGE RecursiveDo, PatternSynonyms, FlexibleContexts                                 #-}
module Commands.Sugar.Keys where
import           Commands.Extra
import           Commands.Backends.OSX
import           Commands.Parsers.Earley (EarleyEither,fullParsesE)

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
safeKbd cs = fullParsesE gKeychords (strip cs)
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
gKeychords :: E.Grammar r (E.Prod r String Char KeyRiff)
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

  <|> SimpleKeyChord SpaceKey                             <$  E.word "<spc>"
  <|> SimpleKeyChord TabKey                               <$  E.word "<tab>"
  <|> SimpleKeyChord ReturnKey                            <$  E.word "<ret>"
  <|> SimpleKeyChord DeleteKey                            <$  E.word "<del>"
  <|> SimpleKeyChord EscapeKey                            <$  E.word "<esc>"

  <|> SimpleKeyChord UpArrowKey     <$ E.word  "<up>"
  <|> SimpleKeyChord DownArrowKey   <$ E.word  "<down>"
  <|> SimpleKeyChord LeftArrowKey   <$ E.word  "<left>"
  <|> SimpleKeyChord RightArrowKey  <$ E.word  "<right>"

  <|> SimpleKeyChord F1Key <$ E.word  "<f1>"
  <|> SimpleKeyChord F2Key <$ E.word  "<f2>"
  <|> SimpleKeyChord F3Key <$ E.word  "<f3>"
  <|> SimpleKeyChord F4Key <$ E.word  "<f4>"
  <|> SimpleKeyChord F5Key <$ E.word  "<f5>"
  <|> SimpleKeyChord F6Key <$ E.word  "<f6>"
  <|> SimpleKeyChord F7Key <$ E.word  "<f7>"
  <|> SimpleKeyChord F8Key <$ E.word  "<f8>"
  <|> SimpleKeyChord F9Key <$ E.word  "<f9>"
  <|> SimpleKeyChord F10Key <$ E.word  "<f10>"
  <|> SimpleKeyChord F11Key <$ E.word  "<f11>"
  <|> SimpleKeyChord F12Key <$ E.word  "<f12>"
  <|> SimpleKeyChord F13Key <$ E.word  "<f13>"
  <|> SimpleKeyChord F14Key <$ E.word  "<f14>"
  <|> SimpleKeyChord F15Key <$ E.word  "<f15>"
  <|> SimpleKeyChord F16Key <$ E.word  "<f16>"
  <|> SimpleKeyChord F17Key <$ E.word  "<f17>"
  <|> SimpleKeyChord F18Key <$ E.word  "<f18>"
  <|> SimpleKeyChord F19Key <$ E.word  "<f19>"
  <|> SimpleKeyChord F20Key <$ E.word  "<f20>"

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
