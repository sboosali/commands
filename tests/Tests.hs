{-# LANGUAGE LambdaCase #-}
module Main where
import Commands.Test.Properties
import Data.Monoid              ((<>))
import Test.DocTest
import Test.Tasty
import Test.Tasty.QuickCheck


tastytest = defaultMain $ testGroup "QuickCheck"
 [ testProperty "serialized DNSGrammar is valid Python" prop_DNSGrammar
 ]


{- object file needed because:

@
ByteCodeLink: can't find label
During interactive linking, GHCi couldn't find the following symbol:
  currentApplicationPath
This may be due to you not asking GHCi to load extra object files,
archives or DLLs needed by your current session.
@

after adding @"dist/build/cbits/objc_actor.o"@, still doesn't work:

@
ghc: panic! (the 'impossible' happened)
  (GHC version 7.8.3 for x86_64-apple-darwin):
  Loading temp shared object failed: dlopen(/var/folders/2z/3c9t_c2d22175v217mf5znsh0000gn/T/ghc96820_0/ghc96820_1.dylib, 9): Symbol not found: _NSStringPboardType
  Referenced from: /var/folders/2z/3c9t_c2d22175v217mf5znsh0000gn/T/ghc96820_0/ghc96820_1.dylib
  Expected in: flat namespace
 in /var/folders/2z/3c9t_c2d22175v217mf5znsh0000gn/T/ghc96820_0/ghc96820_1.dylib
@

even with: @["-frameworkCocoa", "-optl-ObjC"]@

-}
doctestArgs = [ "-frameworkCocoa", "-optl-ObjC", "dist/build/cbits/objc_actor.o" ] <> modules

modules = fmap ("sources/" <>) . (fmap.fmap) (\case '.' -> '/'; c -> c) $

  [ "Commands.Core"

  , "Commands.Symbol"
  , "Commands.Symbol.Types"

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

  , "Commands.Backends.OSX"
  , "Commands.Backends.OSX.Example"
  , "Commands.Backends.OSX.Types"
  , "Commands.Backends.OSX.Bindings"
  , "Commands.Backends.OSX.Bindings.Raw"
  , "Commands.Backends.OSX.Constants"
  , "Commands.Backends.OSX.Marshall"
  , "Commands.Backends.OSX.Execute"
  , "Commands.Backends.OSX.DSL"

  , "Commands.Servers.Servant"
  , "Commands.Servers.Servant.Types"
  , "Commands.Servers.Servant.API"

  , "Commands.DNS13OSX9"
  , "Commands.DNS13OSX9.Types"
  , "Commands.DNS13OSX9.Primitive"
  , "Commands.DNS13OSX9.Combinator"

  , "Commands.Plugins.Example"
  , "Commands.Plugins.Example.Phrase"

  , "Commands.Sugar"
  , "Commands.Sugar.Press"

  , "Commands.Etc"
  , "Commands.Etc.Generics"
  , "Commands.Instances"
  , "Commands.Munging"
  , "Commands.Graph"
  , "Control.Alternative.Free.Associated"
  ]

main = do
 -- doctest doctestArgs
 tastytest
