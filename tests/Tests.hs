{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main where
import Commands.Test.Properties
import Test.DocTest.Discover

import Test.DocTest
import Test.Tasty
import Test.Tasty.QuickCheck

import System.Exit              (exitFailure)


main = do
 doctests  -- ByteCodeLink: can't find label. During interactive linking, GHCi couldn't find the following symbol: currentApplicationPath
 tastytests

tastytests = defaultMain $ testGroup "QuickCheck"
 [ testProperty "serialized DNSGrammar is valid Python" prop_DNSGrammar
 ]

-- doctestModules = fmap ("sources/" <>) . (fmap.fmap) (\case '.' -> '/'; c -> c) $

doctests = do
 loadPackageDescription  "./commands-core.cabal" >>= \case
  Left e -> do
   print e
   exitFailure
  Right pkg -> do
   -- print (pkg)
   print (modulePaths pkg)
   doctest (modulePaths pkg)
