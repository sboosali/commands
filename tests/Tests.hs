{-# LANGUAGE LambdaCase #-}
module Main where
import Commands.Test.Properties
import Test.DocTest
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Monoid                   ((<>))


tastytest = defaultMain $ testGroup "QuickCheck"
 [ testProperty "serialized DNSGrammar is valid Python" prop_DNSGrammar
 ]

modules = fmap ("sources/" <>) . (fmap.fmap) (\case '.' -> '/'; c -> c) $

  [ "Commands.Core"

  , "Commands.Etc"
  , "Commands.Instances"

  , "Commands.Grammar.Types"
  , "Commands.Grammar"

  , "Commands.Parse.Types"
  , "Commands.Parse"
  , "Commands.Parsec"

  , "Commands.Frontends.Dragon13"
  , "Commands.Frontends.Dragon13.Types"
  , "Commands.Frontends.Dragon13.Lens"
  , "Commands.Frontends.Dragon13.Text"
  , "Commands.Frontends.Dragon13.Render"
  , "Commands.Frontends.Dragon13.Optimize"
  , "Commands.Frontends.Dragon13.Shim"
  , "Commands.Frontends.Dragon13.Serialize"

  , "Commands.Command"
  , "Commands.Command.Types"
  , "Commands.Command.Sugar"
  , "Commands.Command.Combinator"

  , "Commands.Munging"

  , "Commands.Plugins.Example"

  , "Control.Alternative.Free.Tree"
  , "Commands.Graph"
  ]

main = do
 doctest modules
 -- tastytest TODO
