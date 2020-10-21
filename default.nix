{}:
let
  nixpkgs-20_03 = fetchTarball {
    name   = "nixpkgs-stable-20_03";
    url    = "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz";
    sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  };

  nixpkgs = import nixpkgs-20_03 {};
  haskellPackages = nixpkgs.pkgs.haskellPackages;
in
nixpkgs.pkgs.haskell.packages.ghc865.callPackage ./woods.nix { network = haskellPackages.network_3_1_1_1; haskell-lsp-types = haskellPackages.haskell-lsp-types_0_20_0_0;  }

