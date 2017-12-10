{-# LANGUAGE LambdaCase #-}
module Test.DocTest.Discover where

import CabalLenses.PackageDescription
import Control.Lens
import Distribution.InstalledPackageInfo     (PError)
import Distribution.ModuleName 
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (ParseResult (..),
                                              parsePackageDescription)

import Data.List                             (intercalate,nub)


loadPackageDescription :: FilePath -> IO (Either PError GenericPackageDescription)
loadPackageDescription cabalFile = do
 projectDirectory <- return cabalFile --TODO discover
 cabalFile <- readFile projectDirectory
 return$ case parsePackageDescription cabalFile of
  ParseFailed e -> Left e
  ParseOk _ pkg -> Right pkg

-- | when `hs-source-dirs` is not singleton, includes nonexistent files.
-- Removes duplicates, as stripping ".hs" extension often collides into a directory.
modulePaths :: GenericPackageDescription -> [FilePath]
modulePaths pkg = do
 d <- pkg^..packageHsSourcesDirs
 m <- pkg^..packageExposedModules
 return$ (intercalate "/" . nub) (d : components m) -- e.g. ("sources/sources" : ["Commands","Core"])

-- packageLibrary :: Traversal' GenericPackageDescription Library
-- packageLibrary = packageDescriptionL.libraryL.each

-- | ignores 'condTreeComponentsL'
packageCondLibrary :: Traversal' GenericPackageDescription Library
packageCondLibrary = condLibraryL.each.condTreeDataL

packageExposedModules :: Traversal' GenericPackageDescription ModuleName
packageExposedModules = packageCondLibrary.exposedModulesL.each

packageHsSourcesDirs :: Traversal' GenericPackageDescription FilePath
packageHsSourcesDirs = packageCondLibrary.libBuildInfoL.hsSourceDirsL.each

