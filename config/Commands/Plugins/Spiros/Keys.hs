{-# LANGUAGE LambdaCase, OverloadedStrings, PostfixOperators, RankNTypes #-}
{-# LANGUAGE TemplateHaskell                                #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Keys where
import Commands.Plugins.Spiros.Extra
import           Commands.Plugins.Spiros.Phrase (character)

import Commands.Backends.Workflow as W
import           Commands.Mixins.DNS13OSX9

keyriff :: R KeySequence  -- TODO re name to key sequence
keyriff = 'keyriff
 <=> (keychord-++)

-- | the terminals in key and modifier should be disjoint; otherwise, there is ambiguity.
keychord :: R KeyChord
keychord = 'keychord
 <=> moveShift <$ "press" <*> (modifier-*)  <*> key      -- zero or more modifiers (prefixed)
 <|> moveShift <$>            (modifier-++) <*> key      -- one or more modifiers (not prefixed)
 where
 moveShift ms (ms', k) = KeyChord (ms ++ ms') k

modifier = 'modifier
 <=> "met"   $> MetaModifier
--TODO <|> ""   $> HyperModifier
 <|> "con"   $> ControlModifier
 <|> "shift" $> ShiftModifier
 <|> "alt"   $> OptionModifier
 <|> "fun"   $> FunctionModifier

{- | a non-modifier key

'Key's and 'Char'acters are "incomparable sets":

* many modifiers are keys that aren't characters (e.g. 'CommandKey')
* many nonprintable characters are not keys (e.g. @\'\\0\'@)

so we can't embed the one into the other, but we'll just keep things simple with duplication.

-}
key :: R KeyChord
key = 'key
 <=> ((either __BUG__ id) . char2keychord) <$> (__inlineRHS__(character))
  -- inlined to trigger vocabulary optimization, the right-hand side which must have only tokens

 <|> "up" $> KeyChord [] UpArrowKey
 <|> "down" $> KeyChord [] DownArrowKey
 <|> "left" $> KeyChord [] LeftArrowKey
 <|> "right" $> KeyChord [] RightArrowKey

 <|> "del" $> KeyChord [] DeleteKey
 <|> "cape" $> KeyChord [] EscapeKey
--  <|> functionKey
-- functionKey = empty
 <|> "F1" $> KeyChord [] F1Key
 <|> "F2" $> KeyChord [] F2Key
 <|> "F3" $> KeyChord [] F3Key
 <|> "F4" $> KeyChord [] F4Key
 <|> "F5" $> KeyChord [] F5Key
 <|> "F6" $> KeyChord [] F6Key
 <|> "F7" $> KeyChord [] F7Key
 <|> "F8" $> KeyChord [] F8Key
 <|> "F9" $> KeyChord [] F9Key
 <|> "F10" $> KeyChord [] F10Key
 <|> "F11" $> KeyChord [] F11Key
 <|> "F12" $> KeyChord [] F12Key
 <|> "F13" $> KeyChord [] F13Key
 <|> "F14" $> KeyChord [] F14Key
 <|> "F15" $> KeyChord [] F15Key
 <|> "F16" $> KeyChord [] F16Key
 <|> "F17" $> KeyChord [] F17Key
 <|> "F18" $> KeyChord [] F18Key
 <|> "F19" $> KeyChord [] F19Key
 <|> "F20" $> KeyChord [] F20Key

-- -- functionKey = empty
--  <|> "eff one" $> KeyChord [] F1Key  -- TODO can DNS vocabularies handle strings with multiple tokens? YES
--  <|> "eff two" $> KeyChord [] F2Key
--  <|> "eff three" $> KeyChord [] F3Key
--  <|> "eff four" $> KeyChord [] F4Key
--  <|> "eff five" $> KeyChord [] F5Key
--  <|> "eff six" $> KeyChord [] F6Key
--  <|> "eff seven" $> KeyChord [] F7Key
--  <|> "eff eight" $> KeyChord [] F8Key
--  <|> "eff nine" $> KeyChord [] F9Key
--  <|> "eff ten" $> KeyChord [] F10Key
--  <|> "eff eleven" $> KeyChord [] F11Key
--  <|> "eff twelve" $> KeyChord [] F12Key
--  <|> "eff thirteen" $> KeyChord [] F13Key
--  <|> "eff fourteen" $> KeyChord [] F14Key
--  <|> "eff fifteen" $> KeyChord [] F15Key
--  <|> "eff sixteen" $> KeyChord [] F16Key
--  <|> "eff seventeen" $> KeyChord [] F17Key
--  <|> "eff eighteen" $> KeyChord [] F18Key
--  <|> "eff nineteen" $> KeyChord [] F19Key
--  <|> "eff twenty" $> KeyChord [] F20Key

-- | an ordinal numeral
ordinal :: R Integer
ordinal = 'ordinal
 <=> unOrdinal <$> (__inlineRHS__(ordinalDigit))
 <|> vocab
  [ "tenth"-: 10
  , "eleven"-: 11
  , "twelve"-: 12
  , "thirteen"-: 13
  , "fourteen"-: 14
  , "fifteen"-: 15
  , "sixteen"-: 16
  , "seventeen"-: 17
  , "eighteen"-: 18
  , "nineteenth"-: 19
  , "twentieth"-: 20
  ]

-- | an ordinal numeral, between 0 (zeroth) and 9 (ninth) inclusive
ordinalDigit :: R Ordinal
ordinalDigit = 'ordinalDigit <=> vocab dictOrdinalDigit

dictOrdinalDigit :: [(String, Ordinal)]
dictOrdinalDigit = fmap (fmap Ordinal)
 [ "zeroth"-: 0
 , "first"-: 1
 , "second"-: 2
 , "third"-: 3
 , "fourth"-: 4
 , "fifth"-: 5
 , "sixth"-: 6
 , "seventh"-: 7
 , "eighth"-: 8
 , "ninth"-: 9
 ]

-- Keypress [Meta] CKey
--  OR
-- kbd "M-c"
