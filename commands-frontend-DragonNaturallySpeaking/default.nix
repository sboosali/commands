{ mkDerivation, aeson, base, bifunctors, commands-core, containers
, deepseq, Earley, either, exceptions, free, hashable
, interpolatedstring-perl6, language-python, lens, ListLike, mtl
, profunctors, reducers, semigroups, spiros, split, stdenv
, template-haskell, text, transformers, wl-pprint-text
}:
mkDerivation {
  pname = "commands-frontend-DragonNaturallySpeaking";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bifunctors commands-core containers deepseq Earley
    either exceptions free hashable interpolatedstring-perl6
    language-python lens ListLike mtl profunctors reducers semigroups
    spiros split template-haskell text transformers wl-pprint-text
  ];
  homepage = "http://github.com/sboosali/commands-frontend-DragonNaturallySpeaking#readme";
  description = "TODO";
  license = stdenv.lib.licenses.bsd3;
}
