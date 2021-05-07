{}:
let
  nixpkgs-20_09 = fetchTarball {
    name   = "nixpkgs-stable-20_09";
    url    = "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz";
    sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
  };

  nixpkgs = import nixpkgs-20_09 {};
in {
   env = nixpkgs.stdenv.mkDerivation rec {
     name = "woods-dev";
    buildInputs = [
      nixpkgs.ghc
      nixpkgs.cabal-install
      nixpkgs.protobuf
    ];
  };
}

