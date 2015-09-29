{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DeriveGeneric,  FlexibleInstances, TupleSections, TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances, PatternSynonyms, FlexibleContexts, UndecidableInstances                             #-}
{- | syntactic sugar for defining type-safe keyboard shortcuts, under the 'Actions' monad.

* the @instance PressArg Integer@ is the sugar for the number keys;
as integer literals are polymorphic, you'll need @ExtendedDefaultRules@ and you'll want @-fno-warn-type-defaults@.
* all the alphabetical keys, and a few other common keys, are given identifier aliases;
by polluting the global namespace with single-letter identifiers, your config will want @-fno-warn-name-shadowing@
* the @instance PressArg Char@ and @instance PressArg String@ are the sugar for the rest, characters that are more readable to surround with single quotes than spell out.
* the non-alphanumeric aliased keys (like 'ret') follow the Emacs convention, lowercased.
* use 'S'hift for capital letters: @'press' 'C' 'S' 'b'@.

-}
module Commands.Sugar.Press where -- TODO move aliases to Commands.Sugar.Aliases. They keep polluting the namespace.
import Commands.Etc() 
import Commands.Backends.OSX

-- import Control.Monad.Free (MonadFree)

import Data.Foldable               (traverse_)
import Data.Monoid                 ((<>))


{- |a <http://chris-taylor.github.io/blog/2013/03/01/how-haskell-printf-works/ polyvariadic function>
 that desugars to a sequence of 'sendKeyChord'es.

e.g. @(C-u) x 1@, an Emacs keyboard shortcut, in the DSL:

@
press C \'u\' >> press \'x\' 1
@

e.g. all features (all instances, multiple modifiers):

@
press 'C' 'M' 'tab' 'ZKey' \'O\' \"abc\" 1 (-123)
-- is the same as
press 'Control' 'Command' "zOabc-123"
@

e.g. modifiers without keys (invalid):

@
press Command
-- is the same as
return ()
@

this is a huge hack, but increases the readability of your configuration.
an imperative DSL (here, an expression of type 'Actions ()' using 'press'),
whose commands are triggered by *voice* (e.g. "press command zee"),
is one of the cases I really want to my code to "read like English".

@press = 'pressFun' ([],[])@ 

(takes at least one argument) 
-}
press :: (PressArg a, PressFun f) => a -> f
press = pressFun ([],[])


type PressArgs = ([Modifier], [KeyChord])

{- | its instances can be an argument to 'press'. simply injects into a sum type. 

see 'pressFun':
 
* @Left@ means merge the modifiers together 
* @Right@ means don't merge the modifier (always 'Shift') 

e.g. @'press' M C \'T\' \'x\'@ works on:

@[Left CommandMod, Left Control, Right (KeyChord [Shift] TKey), Right (KeyChord [] XKey)]@ 

-}
class PressArg a
 where toPressArg :: a -> Either Modifier [KeyChord]

instance PressArg Modifier  where toPressArg = Left
instance PressArg KeyRiff   where toPressArg = Right
instance PressArg KeyChord  where toPressArg = Right . (:[])
instance PressArg Key       where toPressArg = Right . (:[]) . NoMod
instance PressArg Char      where toPressArg = Right . char2keypress
-- ^ 
instance PressArg String    where toPressArg = Right . concatMap char2keypress
instance PressArg Integer   where toPressArg = Right . int2keypress
-- any IsString
-- any Num?


class PressFun f where
 pressFun :: PressArgs -> f

{-| the equality constraint @(a ~ ())@ prevents the instance ambiguity error:

@
No instance for (PressFun (Control.Monad.Free.Free ActionF a0)) arising from a use of ‘press’
The type variable ‘a0’ is ambiguous
@

like from the use of an unused call, inferred polymorphic:

@
do
 press command z
 return something
@

since @(Workflow ())@ is the only type for which this instance makes sense, and because these instances are "closed", we don't lose out on any desired instances with this trick.

-}
instance (a ~ ()) => PressFun (Workflow a) where
 pressFun (modifiers, keypresses) = traverse_ (\(ms,k) -> sendKeyChord (modifiers <> ms) k) keypresses

-- instance (a ~ (), MonadFree ActionF m) => PressFun (m a) where
-- the instances benignly overlap. since the function arrow (@(->)@) is a unary constructor, like the Monad (@m@).
-- TODO possible without forcing overlapping influences on client?  

instance (PressArg a, PressFun f) => PressFun (a -> f)  where
 pressFun (modifiers, keypresses) arg = case toPressArg arg of
  Left  m   -> pressFun (modifiers <> [m], keypresses)
  Right kps -> pressFun (modifiers,        keypresses <> kps)

-- ^ alias for 'CommandMod'
pattern M = CommandMod
-- ^ alias for 'Control'
pattern C = Control
-- ^ alias for 'Shift'
pattern S = Shift
-- ^ alias for 'Option'
pattern O = Option
-- ^ alias for 'Function'
pattern F = Function

