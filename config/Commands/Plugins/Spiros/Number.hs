{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, FlexibleInstances, TemplateHaskellQuotes, OverloadedStrings, PostfixOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Number where
import           Commands.Plugins.Spiros.Extra

import           Commands.Mixins.DNS13OSX9

import GHC.Stack

import Digit
import Prelude.Spiros

instance Rankable Digit  -- omg this stupid typeclass must die
instance Rankable Decimal

type Decimal = [Digit]
fromDecimal :: (Integral a) => Decimal -> a
fromDecimal = fmap fromDigit > fromDigits 10

fromDigit :: (Num a) => Digit -> a
fromDigit (Digit i) = fromIntegral i

digitizedNumber  :: R Decimal
digitizedNumber = 'digitizedNumber <=> (digit__ -++)-- TODO remove

digits__ :: R Decimal
digits__ = 'digits__ <=> (digit__ -++)

digit__ :: R Digit-- TODO renamed the one in Correct.Grammar to avoid conflict with this
digit__  = vocab
 [ "zero"  -: Digit 0                 -- disyllabic
 , "oh"    -: Digit 0                 -- sometimes more fluent
-- , "nil"   -: Digit 0                 -- monosyllabic
 , "one"   -: Digit 1
 , "two"   -: Digit 2
 , "three" -: Digit 3
 , "four"  -: Digit 4
 , "five"  -: Digit 5
 , "six"   -: Digit 6
--  , "sev"   -: Digit 7                -- monosyllabic
 , "seven" -: Digit 7                -- disyllabic
 , "eight" -: Digit 8
 , "nine"  -: Digit 9
 ]

--------------------------------------------------------------------------------

englishNumericRHS :: DNSEarleyRHS Char
englishNumericRHS = vocab
 [ "zero"-: '0'
 , "one"-: '1'
 , "two"-: '2'
 , "three"-: '3'
 , "four"-: '4'
 , "five"-: '5'
 , "six"-: '6'
 , "seven"-: '7'
 , "eight"-: '8'
 , "nine"-: '9'
 ]

-- | @('read' <$> digits :: R_ 'Int')@ is total.
digits :: R String
digits = 'digits <=> (digit-++) -- TODO

digit :: (HasCallStack) => R Char
digit = 'digit <=> (head . show) <$> digitRHS -- TODO errorWithCallStack

digitRHS :: (Num a) => R a
digitRHS = vocab
 [ "nil"   -: 0                  -- monosyllabic
 , "zero"  -: 0                 -- disyllabic
 , "one"   -: 1
 , "two"   -: 2
 , "three" -: 3
 , "four"  -: 4
 , "five"  -: 5
 , "six"   -: 6
 , "sev"   -: 7                  -- monosyllabic
 , "seven" -: 7                -- disyllabic
 , "eight" -: 8
 , "nine"  -: 9
 ]

--------------------------------------------------------------------------------

number :: R Number
number = 'number <=> numberRHS

numberRHS :: (Num a) => R a
numberRHS = digitRHS <|> vocab
 [ "ten"-: 10
 , "eleven"-: 11
 , "twelve"-: 12
 , "thirteen"-: 13
 , "fourteen"-: 14
 , "fifteen"-: 15
 , "sixteen"-: 16
 , "seventeen"-: 17
 , "eighteen"-: 18
 , "nineteen"-: 19
 , "twenty"-: 20
 , "twenty-one"-: 21
 , "twenty-two"-: 22
 , "twenty-three"-: 23
 , "twenty-four"-: 24
 , "twenty-five"-: 25
 , "twenty-six"-: 26
 , "twenty-seven"-: 27
 , "twenty-eight"-: 28
 , "twenty-nine"-: 29
 , "thirty"-: 30
 , "thirty-one"-: 31
 , "thirty-two"-: 32
 , "thirty-three"-: 33
 , "thirty-four"-: 34
 , "thirty-five"-: 35
 , "thirty-six"-: 36
 , "thirty-seven"-: 37
 , "thirty-eight"-: 38
 , "thirty-nine"-: 39
 , "forty"-: 40
 , "forty-one"-: 41
 , "forty-two"-: 42
 , "forty-three"-: 43
 , "forty-four"-: 44
 , "forty-five"-: 45
 , "forty-six"-: 46
 , "forty-seven"-: 47
 , "forty-eight"-: 48
 , "forty-nine"-: 49
 , "fifty"-: 50
 , "fifty-one"-: 51
 , "fifty-two"-: 52
 , "fifty-three"-: 53
 , "fifty-four"-: 54
 , "fifty-five"-: 55
 , "fifty-six"-: 56
 , "fifty-seven"-: 57
 , "fifty-eight"-: 58
 , "fifty-nine"-: 59
 , "sixty"-: 60
 , "sixty-one"-: 61
 , "sixty-two"-: 62
 , "sixty-three"-: 63
 , "sixty-four"-: 64
 , "sixty-five"-: 65
 , "sixty-six"-: 66
 , "sixty-seven"-: 67
 , "sixty-eight"-: 68
 , "sixty-nine"-: 69
 , "seventy"-: 70
 , "seventy-one"-: 71
 , "seventy-two"-: 72
 , "seventy-three"-: 73
 , "seventy-four"-: 74
 , "seventy-five"-: 75
 , "seventy-six"-: 76
 , "seventy-seven"-: 77
 , "seventy-eight"-: 78
 , "seventy-nine"-: 79
 , "eighty"-: 80
 , "eighty-one"-: 81
 , "eighty-two"-: 82
 , "eighty-three"-: 83
 , "eighty-four"-: 84
 , "eighty-five"-: 85
 , "eighty-six"-: 86
 , "eighty-seven"-: 87
 , "eighty-eight"-: 88
 , "eighty-nine"-: 89
 , "ninety"-: 90
 , "ninety-one"-: 91
 , "ninety-two"-: 92
 , "ninety-three"-: 93
 , "ninety-four"-: 94
 , "ninety-five"-: 95
 , "ninety-six"-: 96
 , "ninety-seven"-: 97
 , "ninety-eight"-: 98
 , "ninety-nine"-: 99
 , "one-hundred"-: 100
 ]
