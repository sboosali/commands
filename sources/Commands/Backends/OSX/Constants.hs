module Commands.Backends.OSX.Constants where
import Commands.Backends.OSX.Types

import Data.BitVector


-- | line 236 of </System/Library/Frameworks/IOKit.framework/Versions/A/Headers/hidsystem/IOLLEvent.h>
--
--
mask :: Modifier -> BitVector
mask Command  = 0x00100000
mask Control  = 0x00040000
mask Shift    = 0x00020000
mask Option   = 0x00080000
mask Function = 0x00800000

-- | line 196 of </System/Library/Frameworks/Carbon.framework/Versions/A/Frameworks/HIToolbox.framework/Versions/A/Headers/Events.h>
--
keycode :: Key -> BitVector
keycode AKey             = 0x00
keycode SKey             = 0x01
keycode DKey             = 0x02
keycode FKey             = 0x03
keycode HKey             = 0x04
keycode GKey             = 0x05
keycode ZKey             = 0x06
keycode XKey             = 0x07
keycode CKey             = 0x08
keycode VKey             = 0x09
keycode BKey             = 0x0B
keycode QKey             = 0x0C
keycode WKey             = 0x0D
keycode EKey             = 0x0E
keycode RKey             = 0x0F
keycode YKey             = 0x10
keycode TKey             = 0x11
keycode OneKey           = 0x12
keycode TwoKey           = 0x13
keycode ThreeKey         = 0x14
keycode FourKey          = 0x15
keycode SixKey           = 0x16
keycode FiveKey          = 0x17
keycode EqualKey         = 0x18
keycode NineKey          = 0x19
keycode SevenKey         = 0x1A
keycode MinusKey         = 0x1B
keycode EightKey         = 0x1C
keycode ZeroKey          = 0x1D
keycode RightBracketKey  = 0x1E
keycode OKey             = 0x1F
keycode UKey             = 0x20
keycode LeftBracketKey   = 0x21
keycode IKey             = 0x22
keycode PKey             = 0x23
keycode LKey             = 0x25
keycode JKey             = 0x26
keycode QuoteKey         = 0x27
keycode KKey             = 0x28
keycode SemicolonKey     = 0x29
keycode BackslashKey     = 0x2A
keycode CommaKey         = 0x2B
keycode SlashKey         = 0x2C
keycode NKey             = 0x2D
keycode MKey             = 0x2E
keycode PeriodKey        = 0x2F
keycode GraveKey         = 0x32
keycode ReturnKey        = 0x24
keycode TabKey           = 0x30
keycode SpaceKey         = 0x31
keycode DeleteKey        = 0x33
keycode EscapeKey        = 0x35
keycode CommandKey       = 0x37
keycode ShiftKey         = 0x38
keycode CapsLockKey      = 0x39
keycode OptionKey        = 0x3A
keycode ControlKey       = 0x3B
keycode FunctionKey      = 0x3F
keycode F17Key           = 0x40
keycode F18Key           = 0x4F
keycode F19Key           = 0x50
keycode F20Key           = 0x5A
keycode F5Key            = 0x60
keycode F6Key            = 0x61
keycode F7Key            = 0x62
keycode F3Key            = 0x63
keycode F8Key            = 0x64
keycode F9Key            = 0x65
keycode F11Key           = 0x67
keycode F13Key           = 0x69
keycode F16Key           = 0x6A
keycode F14Key           = 0x6B
keycode F10Key           = 0x6D
keycode F12Key           = 0x6F
keycode F15Key           = 0x71
keycode ForwardDeleteKey = 0x75
keycode F4Key            = 0x76
keycode F2Key            = 0x78
keycode F1Key            = 0x7A
keycode LeftArrowKey     = 0x7B
keycode RightArrowKey    = 0x7C
keycode DownArrowKey     = 0x7D
keycode UpArrowKey       = 0x7E

