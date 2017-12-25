{ mkDerivation, aeson, async, base, bytestring, clock
, commands-core, commands-frontend-DragonNaturallySpeaking
, commands-server-types, containers, data-default, deepseq, Earley
, either, enumerate, exceptions, filepath, free, ghc-prim
, haskell-src-meta, interpolatedstring-perl6, language-python, lens
, mtl, parallel, process, s-expression, semigroups, spiros, split
, stdenv, template-haskell, text, time, transformers, vinyl, wai
, warp, wl-pprint-text, workflow-extra, workflow-pure
, workflow-types
}:
mkDerivation {
  pname = "commands-spiros";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring clock commands-core
    commands-frontend-DragonNaturallySpeaking commands-server-types
    containers data-default deepseq Earley either enumerate exceptions
    filepath free ghc-prim haskell-src-meta interpolatedstring-perl6
    language-python lens mtl parallel process s-expression semigroups
    spiros split template-haskell text time transformers vinyl wai warp
    wl-pprint-text workflow-extra workflow-pure workflow-types
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/sboosali/commands-spiros#readme";
  description = "my configuration for commands";
  license = stdenv.lib.licenses.gpl3;
}
