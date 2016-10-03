{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main where
import Test.DocTest

main = doctest
 [ "sources/Commands/Frontends/Dragon13/Serialize.hs" -- Commands.Frontends.Dragon13.Serialize
 ]
