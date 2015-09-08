{-# LANGUAGE LambdaCase, OverloadedStrings, PostfixOperators, RankNTypes #-}
{-# LANGUAGE RecursiveDo, TemplateHaskell                                #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Example.Keys where
import           Commands.Backends.OSX
import           Commands.Etc
import           Commands.Mixins.DNS13OSX9
import           Commands.Plugins.Example.Phrase (character)

import qualified Data.List.NonEmpty              as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import qualified Text.Earley                     as E

import           Control.Applicative
import           Data.Char hiding(Control)
import           Data.Foldable                   (traverse_)


type KeyRiff  = [KeyChord]
type KeyChord = KeyPress

-- | a riff is some chords?
keyriff :: R z KeyRiff
keyriff = 'keyriff
 <=> "press" *> (keychord-++)

-- | the terminals in key and modifier are disjoint; otherwise, there is ambiguity.
keychord :: R z KeyChord
keychord = 'keychord
 <=> moveShift <$> (modifier-*) <*> key
 where
 moveShift ms (ms', k) = KeyPress (ms ++ ms') k

modifier = 'modifier
 <=> "met"   $> CommandMod
 <|> "con"   $> Control
 <|> "shift" $> Shift
 <|> "alt"   $> Option
 <|> "fun"   $> Function

{- | a non-modifier key

'Key's and 'Char'acters are "incomparable sets":

* many modifiers are keys that aren't characters (e.g. 'CommandKey')
* many nonprintable characters are not keys (e.g. @\'\\0\'@)

so we can't embed the one into the other, but we'll just keep things simple with duplication.

-}
key :: R z KeyPress
key = 'key
 <=> ((either __BUG__ id) . char2keypress) <$> (inlineRHS(character))
  -- inlined to trigger vocabulary optimization, the right-hand side which must have only tokens

 <|> "up" $> KeyPress [] UpArrowKey
 <|> "down" $> KeyPress [] DownArrowKey
 <|> "left" $> KeyPress [] LeftArrowKey
 <|> "right" $> KeyPress [] RightArrowKey

 <|> "del" $> KeyPress [] DeleteKey
 <|> "cape" $> KeyPress [] EscapeKey
--  <|> functionKey
-- functionKey = empty
 <|> "F1" $> KeyPress [] F1Key
 <|> "F2" $> KeyPress [] F2Key
 <|> "F3" $> KeyPress [] F3Key
 <|> "F4" $> KeyPress [] F4Key
 <|> "F5" $> KeyPress [] F5Key
 <|> "F6" $> KeyPress [] F6Key
 <|> "F7" $> KeyPress [] F7Key
 <|> "F8" $> KeyPress [] F8Key
 <|> "F9" $> KeyPress [] F9Key
 <|> "F10" $> KeyPress [] F10Key
 <|> "F11" $> KeyPress [] F11Key
 <|> "F12" $> KeyPress [] F12Key
 <|> "F13" $> KeyPress [] F13Key
 <|> "F14" $> KeyPress [] F14Key
 <|> "F15" $> KeyPress [] F15Key
 <|> "F16" $> KeyPress [] F16Key
 <|> "F17" $> KeyPress [] F17Key
 <|> "F18" $> KeyPress [] F18Key
 <|> "F19" $> KeyPress [] F19Key
 <|> "F20" $> KeyPress [] F20Key

-- -- functionKey = empty
--  <|> "eff one" $> KeyPress [] F1Key  -- can DNS vocabularies handle strings with multiple tokens?
--  <|> "eff two" $> KeyPress [] F2Key
--  <|> "eff three" $> KeyPress [] F3Key
--  <|> "eff four" $> KeyPress [] F4Key
--  <|> "eff five" $> KeyPress [] F5Key
--  <|> "eff six" $> KeyPress [] F6Key
--  <|> "eff seven" $> KeyPress [] F7Key
--  <|> "eff eight" $> KeyPress [] F8Key
--  <|> "eff nine" $> KeyPress [] F9Key
--  <|> "eff ten" $> KeyPress [] F10Key
--  <|> "eff eleven" $> KeyPress [] F11Key
--  <|> "eff twelve" $> KeyPress [] F12Key
--  <|> "eff thirteen" $> KeyPress [] F13Key
--  <|> "eff fourteen" $> KeyPress [] F14Key
--  <|> "eff fifteen" $> KeyPress [] F15Key
--  <|> "eff sixteen" $> KeyPress [] F16Key
--  <|> "eff seventeen" $> KeyPress [] F17Key
--  <|> "eff eighteen" $> KeyPress [] F18Key
--  <|> "eff nineteen" $> KeyPress [] F19Key
--  <|> "eff twenty" $> KeyPress [] F20Key

-- | an ordinal numeral
ordinal = 'ordinal
 <=> (1::Int) <$ "one"
 <|> 2 <$ "two"
 <|> 3 <$ "three"
 <|> 4 <$ "four"
 <|> 5 <$ "five"
 <|> 6 <$ "six"
 <|> 7 <$ "seven"
 <|> 8 <$ "eight"
 <|> 9 <$ "nine"
 <|> 10 <$ "ten"
 <|> 11 <$ "eleven"
 <|> 12 <$ "twelve"
 <|> 13 <$ "thirteen"
 <|> 14 <$ "fourteen"
 <|> 15 <$ "fifteen"
 <|> 16 <$ "sixteen"
 <|> 17 <$ "seventeen"
 <|> 18 <$ "eighteen"
 <|> 19 <$ "nineteen"
 <|> 20 <$ "twenty"

-- Keypress [Meta] CKey
--  OR
-- keys M c



-- ================================================================ --

runKeyRiff :: KeyRiff -> Actions_
runKeyRiff = traverse_ (\(KeyPress mods k) -> sendKeyPress mods k)

{- | parser for Emacs-like keybindings syntax.

partial function.

>>> keys"a"
[([],AKey)]

>>> keys"C-M-b"
[([Control,CommandMod],BKey)]

>>> keys"M-S-a"
[([CommandMod, Shift],AKey)]

>>> keys"C-<tab>"
[([CommandMod],TabKey)]

>>> keys"A-\\"
[([Option],BackslashKey)]

>>> keys"C-x o C-x b"
[([Control],XKey),([],OKey),([Control],XKey),([],BKey)]

a partial function, wraps 'safeKeys'. 
-}
keys :: [Char] -> KeyRiff
keys cs = either (error.(("(Commands.Plugins.Example.Keys.keys "++(show cs)++")") ++).show) NonEmpty.head (safeKeys cs)

safeKeys :: [Char] -> EarleyEither String Char (NonEmpty KeyRiff)
safeKeys cs = toEarleyEither $ E.fullParses (E.parser gKeychords (strip cs))
-- bimapEither toException .
 where
 strip :: String -> String
 strip = rstrip . lstrip
 lstrip = dropWhile (`elem` (" \t\n\r"::String))
 rstrip = reverse . lstrip . reverse

-- EarleyEither String Char [KeyChord]

-- | follows (a subset of) <http://emacswiki.org/emacs/EmacsKeyNotation Emacs keybinding syntax>. doesn't interpret uppercase non-modifier letters as shifted alphanumerics, for disambiguation. e.g. use @"M-S-a"@, not @"M-A"@. non-alphanumerics are in angle brackets. e.g. @"C-<tab>"@, not @"C-TAB"@. see source (of readable parser combinators) for other differences.
gKeychords :: E.Grammar r String (E.Prod r String Char KeyRiff)
gKeychords = mdo
 pKeychords <- E.rule$ (:) <$> pKeychord <*> many (some (E.symbol ' ') *> pKeychord)
               E.<?> "keychords"
 pKeychord  <- E.rule$ toKeychord <$> many (pModifier <* E.symbol '-') <*> pKey
               E.<?> "keychord"
 pKey       <- E.rule$ empty

  <|> NoMod SpaceKey                             <$  E.word "<spc>"
  <|> NoMod TabKey                               <$  E.word "<tab>"
  <|> NoMod ReturnKey                            <$  E.word "<ret>"
  <|> NoMod DeleteKey                            <$  E.word "<del>"
  <|> NoMod EscapeKey                            <$  E.word "<esc>"

  <|> NoMod UpArrowKey   <$ E.word  "<up>"
  <|> NoMod DownArrowKey   <$ E.word  "<down>" 
  <|> NoMod LeftArrowKey   <$ E.word  "<left>" 
  <|> NoMod RightArrowKey   <$ E.word  "<right>" 

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

  <|> (either __BUG__ id . char2keypress) <$> E.satisfy ((&&) <$> (not.isAlphaNum) <*> isAscii)   -- must be last 
  <|> (either __BUG__ id . char2keypress) <$> E.satisfy ((&&) <$> isLower <*> isAscii)   -- must be last 

  E.<?> "key"

 pModifier  <- E.rule$ empty
  <|> E.symbol 'M' $> CommandMod  -- generally, the meta-key
  <|> E.symbol 'C' $> Control
  <|> E.symbol 'S' $> Shift
  <|> E.symbol 'A' $> Option
  E.<?> "modifier"
 return pKeychords
 where
 toKeychord = lAppendModifiers
 lAppendModifiers ms (ms', k) = KeyPress (ms ++ ms') k

-- TODO a numerate all keybindings with one modifier and one non-modifier key, it's for documentation
