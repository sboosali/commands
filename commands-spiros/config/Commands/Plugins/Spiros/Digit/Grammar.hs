{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}
module Commands.Plugins.Spiros.Digit.Grammar where
import Commands.Plugins.Spiros.Extra

import Commands.Mixins.DNS13OSX9 -- TODO shorter module name

import Digit


-- TODO use digits everywhere, ordinal grammars and cardinal garments

digit_ :: R Digit
digit_ = vocabWith Digit
 [ "zero"  -: 0                -- disyllabic
 , "one"   -: 1
 , "two"   -: 2
 , "three" -: 3
 , "four"  -: 4
 , "five"  -: 5
 , "six"   -: 6
 , "seven" -: 7                -- disyllabic
 , "eight" -: 8
 , "nine"  -: 9
 ]
