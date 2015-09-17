{-# LANGUAGE LambdaCase, OverloadedStrings, PostfixOperators, RankNTypes #-}
{-# LANGUAGE TemplateHaskell                                #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Example.Keys where
import           Commands.Backends.OSX
import           Commands.Etc
import           Commands.Mixins.DNS13OSX9
import           Commands.Plugins.Example.Phrase (character)

import           Control.Applicative
import           Data.Foldable                   (traverse_)


-- | a riff is some chords?
keyriff :: R z KeyRiff
keyriff = 'keyriff
 <=> (keychord-++)

-- | the terminals in key and modifier should be disjoint; otherwise, there is ambiguity.
keychord :: R z KeyChord
keychord = 'keychord
 <=> moveShift <$ "press" <*> (modifier-*)  <*> key      -- zero or more modifiers (prefixed) 
 <|> moveShift <$>            (modifier-++) <*> key      -- one or more modifiers (not prefixed) 
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

runKeyRiff :: KeyRiff -> Actions_
runKeyRiff = traverse_ (\(KeyPress mods k) -> sendKeyPress mods k)

