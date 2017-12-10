{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, bytestring, Cabal, containers
      , doctest, Earley, either, exceptions, filepath, free, ghc-prim
      , hashable, http-types, interpolatedstring-perl6, kan-extensions
      , lens, ListLike, MemoTrie, mtl, parallel, pipes, profunctors
      , QuickCheck, recursion-schemes, reducers, reified-bindings
      , semigroups, servant-client, spiros, split, stdenv, stm, tagged
      , tasty, tasty-quickcheck, template-haskell, text, time
      , transformers, unordered-containers, vector
      }:
      mkDerivation {
        pname = "commands-core";
        version = "0.0.0";
        sha256 = "0";
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          async base bytestring Cabal containers Earley either exceptions
          filepath free ghc-prim hashable http-types interpolatedstring-perl6
          kan-extensions lens ListLike MemoTrie mtl parallel pipes
          profunctors recursion-schemes reducers reified-bindings semigroups
          servant-client spiros split stm tagged template-haskell text time
          transformers unordered-containers vector
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [
          base Cabal doctest lens QuickCheck semigroups tasty
          tasty-quickcheck template-haskell text
        ];
        homepage = "https://github.com/sboosali/commands-core#readme";
        description = "control your computer by voice, with \"grammar combinators\"";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
		       
  drv = haskellPackages.callPackage f {
   reified-bindings = ../reified-bindings;
   spiros = ../spiros; 
   };

in

  if pkgs.lib.inNixShell then drv.env else drv
