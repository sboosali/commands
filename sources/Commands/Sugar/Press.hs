{-# LANGUAGE FlexibleInstances, TupleSections, TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances                           #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{- | syntactic sugar for defining type-safe keyboard shortcuts, under the 'Actions' monad.


* the @instance PressArg Integer@ is the sugar for the number keys;
as integer literals are polymorphic, you'll need @ExtendedDefaultRules@ and you'll want @-fno-warn-type-defaults@.
* all the alphabetical keys, and a few other common keys, are given identifier aliases;
by polluting the global namespace with single-letter identifiers, your config will want @-fno-warn-name-shadowing@
* the @instance PressArg Char@ and @instance PressArg String@ are the sugar for the rest, characters that are more readable to surround with single quotes than spell out.
* the non-alphanumeric aliased keys (like 'ret') follow the Emacs convention, lowercased.
* use 'S'hift for capital letters: @'press' 'C' 'S' 'b'@.

-}
module Commands.Sugar.Press where
import Commands.Backends.OSX.DSL
import Commands.Backends.OSX.Types

import Data.Foldable               (traverse_)
import Data.Monoid                 ((<>))


{- | desugars to a sequence of 'sendKeyPress'es

e.g. @(C-u) x 1@, an Emacs keyboard shortcut, in the DSL:

@
press C u >> press x 1
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
is one of the few cases I really want to my code to "read like English".


-}
press :: (PressArg a, PressFun f) => a -> f
press = pressFun ([],[])


type PressArgs = ([Modifier], [KeyPress])
type PressArgT = Either Modifier [KeyPress]

-- | its instances can be an argument to 'press'. simply injects into a sum type.
class PressArg a where toPressArg :: a -> PressArgT

instance PressArg Modifier  where toPressArg = Left
instance PressArg ModifierSynonym  where toPressArg = Left . pattern
instance PressArg Key       where toPressArg = Right . (:[]) . ([],)
instance PressArg Char      where toPressArg = Right . char2keypress
instance PressArg String    where toPressArg = Right . concatMap char2keypress
instance PressArg Integer       where toPressArg = Right . int2keypress
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

since @(Actions ())@ is the only type for which this instance makes sense, and because these instances are "closed", we don't lose out on any desired instances with this trick.

-}
instance (a ~ ()) => PressFun (Actions a) where
 pressFun (modifiers, keypresses) = traverse_ (\(ms,k) -> sendKeyPress (modifiers <> ms) k) keypresses

instance (PressArg a, PressFun f) => PressFun (a -> f)  where
 pressFun (modifiers, keypresses) arg = case toPressArg arg of
  Left  m   -> pressFun (modifiers <> [m], keypresses)
  Right kps -> pressFun (modifiers,        keypresses <> kps)

-- | exported @PatternSynonyms@ breaks (my version of) Haddock/GHC (something about the interface files). once fixed, I will replace this type with:
--
-- @
-- pattern M = Command
-- pattern C = Control
-- pattern S = Shift
-- pattern O = Option
-- pattern F = Function
-- @
--
--
data ModifierSynonym = M | C | S | O | F

pattern :: ModifierSynonym -> Modifier
pattern M = Command
pattern C = Control
pattern S = Shift
pattern O = Option
pattern F = Function

-- aliases

met = Command
ctrl = Control
shift = Shift
alt = Option

del = DeleteKey
tab = TabKey
spc = SpaceKey
ret = ReturnKey
esc = EscapeKey

left = LeftArrowKey
right = RightArrowKey
down = DownArrowKey
up = UpArrowKey

f1 = F1Key
f2 = F2Key
f3 = F3Key
f4 = F4Key
f5 = F5Key
f6 = F6Key
f7 = F7Key
f8 = F8Key
f9 = F9Key
f10 = F10Key
f11 = F11Key
f12 = F12Key
f13 = F13Key
f14 = F14Key
f15 = F15Key
f16 = F16Key
f17 = F17Key
f18 = F18Key
f19 = F19Key
f20 = F20Key

a = AKey
b = BKey
c = CKey
d = DKey
e = EKey
f = FKey
g = GKey
h = HKey
i = IKey
j = JKey
k = KKey
l = LKey
m = MKey
n = NKey
o = OKey
p = PKey
q = QKey
r = RKey
s = SKey
t = TKey
u = UKey
v = VKey
w = WKey
x = XKey
y = YKey
z = ZKey
