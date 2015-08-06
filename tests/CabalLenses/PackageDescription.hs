{-# LANGUAGE TemplateHaskell #-}

-- |
-- Lenses for several data types of the 'Distribution.PackageDescription' module.
-- All lenses are named after their field names with a 'L' appended.

module CabalLenses.PackageDescription where
import CabalLenses.PackageDescription.TH

import Control.Lens
import Distribution.PackageDescription   (Benchmark (..), BuildInfo (..),
                                          CondTree (..), Executable (..),
                                          GenericPackageDescription (..),
                                          Library (..), PackageDescription (..),
                                          TestSuite (..))


makeLensesWith suffixedFields ''GenericPackageDescription
makeLensesWith suffixedFields ''PackageDescription
makeLensesWith suffixedFields ''Library
makeLensesWith suffixedFields ''Executable
makeLensesWith suffixedFields ''TestSuite
makeLensesWith suffixedFields ''Benchmark
makeLensesWith suffixedFields ''BuildInfo
makeLensesWith suffixedFields ''CondTree
