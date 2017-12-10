{ mkDerivation, base, containers, servant, servant-server, spiros
, stdenv, text, transformers, wai, warp, workflow-types
}:
mkDerivation {
  pname = "commands-server-simple";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers servant servant-server spiros text transformers wai
    warp workflow-types
  ];
  executableHaskellDepends = [ base ];
  homepage = "http://github.com/sboosali/commands-server-simple#readme";
  description = "A very simple @servant@ HTTP server that executes @WorkflowT IO@ actions";
  license = stdenv.lib.licenses.bsd3;
}
