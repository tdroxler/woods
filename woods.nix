{ mkDerivation, aeson, attoparsec, base, bytestring, Cabal
, directory, filemanip, filepath, haskell-lsp-types, hspec
, hspec-discover, microlens, network, proto-lens, proto-lens-protoc
, proto-lens-runtime, proto-lens-setup, stdenv, text, pkgs
}:
mkDerivation {
  pname = "woods";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal proto-lens-setup ];
  buildTools = [ pkgs.protobuf ];
  libraryHaskellDepends = [
    aeson attoparsec base bytestring directory filemanip filepath
    haskell-lsp-types microlens network proto-lens proto-lens-runtime
    text
  ];
  libraryToolDepends = [ proto-lens-protoc ];
  executableHaskellDepends = [
    aeson base bytestring haskell-lsp-types microlens network
    proto-lens proto-lens-runtime text
  ];
  testHaskellDepends = [
    base bytestring directory haskell-lsp-types hspec microlens
    proto-lens text
  ];
  testToolDepends = [ hspec-discover ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
