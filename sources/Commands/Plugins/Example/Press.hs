{-# LANGUAGE  TemplateHaskell, OverloadedStrings, PostfixOperators, RankNTypes, RecursiveDo, LambdaCase       #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Example.Press where
import Commands.Etc
import Commands.Backends.OSX
import Commands.Mixins.DNS13OSX9
import Commands.Plugins.Example.Phrase (character)

import qualified Text.Earley                  as E
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text.Lazy                 (Text)

import Control.Applicative
import Data.Foldable (traverse_)
import Data.Char (isAlphaNum)
import           GHC.Exts                        (IsString (..))


type KeyRiff  = [KeyChord]
type KeyChord = KeyPress
-- pattern = KeyPress [] PatternSynonyms

-- TODO global context (e.g. all Apps) should be overridden by a local context (e.g. some App)
myShortcuts = 'myShortcuts <=> shortcuts
 [ "undo"-: "M-z"
 -- , ""-: ""
 , "salt"-: "M-a"
 ]
 -- <|> "copy" $> [KeyPress [CommandMod] CKey]
 -- <|> "copy" $> keys"M-c"

shortcuts :: (Functor'RHS n Text f) => [(String,String)] -> RHS n Text f KeyRiff
shortcuts = foldMap $ \case
 ("","") -> empty               -- for convenience
 (s, k) -> keys k <$ fromString s

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

-- | a non-modifier key
--
-- 'Key's and 'Char'acters are "incomparable sets":
--
-- * many modifiers are keys that aren't characters (e.g. 'CommandKey')
-- * many nonprintable characters are not keys (e.g. @\'\\0\'@)
--
-- so we can't embed the one into the other, but we'll just keep things simple with duplication.
--
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
 <|> "eff one" $> KeyPress [] F1Key  -- can DNS vocabularies handle strings with multiple tokens
 <|> "eff two" $> KeyPress [] F2Key
 <|> "eff three" $> KeyPress [] F3Key
 <|> "eff four" $> KeyPress [] F4Key
 <|> "eff five" $> KeyPress [] F5Key
 <|> "eff six" $> KeyPress [] F6Key
 <|> "eff seven" $> KeyPress [] F7Key
 <|> "eff eight" $> KeyPress [] F8Key
 <|> "eff nine" $> KeyPress [] F9Key
 <|> "eff ten" $> KeyPress [] F10Key
 <|> "eff eleven" $> KeyPress [] F11Key
 <|> "eff twelve" $> KeyPress [] F12Key
 <|> "eff thirteen" $> KeyPress [] F13Key
 <|> "eff fourteen" $> KeyPress [] F14Key
 <|> "eff fifteen" $> KeyPress [] F15Key
 <|> "eff sixteen" $> KeyPress [] F16Key
 <|> "eff seventeen" $> KeyPress [] F17Key
 <|> "eff eighteen" $> KeyPress [] F18Key
 <|> "eff nineteen" $> KeyPress [] F19Key
 <|> "eff twenty" $> KeyPress [] F20Key

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

keys :: [Char] -> KeyRiff
keys cs = either (error.show) NonEmpty.head . toEarleyEither $ E.fullParses (E.parser gKeychords (strip cs))
-- bimapEither toException . 
 where
 strip :: String -> String
 strip = rstrip . lstrip
 lstrip = dropWhile (`elem` (" \t\n\r"::String))
 rstrip = reverse . lstrip . reverse

-- EarleyEither String Char [KeyChord]

gKeychords :: E.Grammar r String (E.Prod r String Char KeyRiff)
gKeychords = mdo
 pKeychords <- E.rule$ (:) <$> pKeychord <*> many (some (E.symbol ' ') *> pKeychord)
 pKeychord  <- E.rule$ toKeychord <$> many (pModifier <* E.symbol '-') <*> pKey E.<?> "keychord"
 pKey       <- E.rule$ E.satisfy isAlphaNum E.<?> "key"
 pModifier  <- E.rule$ empty
  <|> E.symbol 'M' $> CommandMod  -- generally, the meta-key
  <|> E.symbol 'C' $> Control
  <|> E.symbol 'S' $> Shift
  E.<?> "modifier"
 return pKeychords
 where
 toKeychord ms c = lAppendModifiers ms ((either __BUG__ id) (char2keypress c))
 lAppendModifiers ms (ms', k) = KeyPress (ms ++ ms') k

mainPress = do
 -- print$ keys""
 print$ keys"a"
 print$ keys"C-M-b"
 print$ keys"M-S-a"
 print$ keys"M-A"
 print$ keys"C-x o C-x b"
