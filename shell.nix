{nixpkgs ? import (fetchTarball https://github.com/nixos/nixpkgs/archive/nixpkgs-unstable.tar.gz) {}, compiler ? "default", doBenchmark ? false }:

let
  inherit (nixpkgs) pkgs;
  f = { mkDerivation, aeson, base, lib, servant, servant-server
      , wai, wai-cors, warp,
      }:
      mkDerivation {
        pname = "roasted";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base servant servant-server wai wai-cors warp
        ];
        license = lib.licenses.mit;
        mainProgram = "roasted";
      };

  haskellPackages = if compiler == "default" then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = variant (haskellPackages.callPackage f {});

in
  haskellPackages.shellFor {
    packages = hpkgs: [
      # reuse the nixpkgs for this package
      hpkgs.distribution-nixpkgs
      # call our generated Nix expression manually
      drv
    ];
  
    # development tools we use
    nativeBuildInputs = [
      pkgs.cabal-install
      pkgs.haskell-language-server
      pkgs.haskellPackages.doctest
      pkgs.cabal2nix
    ];
        
    # Don't quit know if this is important
    distribution_nixpkgs_datadir = toString ./distribution-nixpkgs;
  }
