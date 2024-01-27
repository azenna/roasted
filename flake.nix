{
  description = "A roasted backend";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/main";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }: flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};
      hdeps = pkgs.haskellPackages.callCabal2nix "roasted" ./roasted.cabal { };
    in {
      devShell = pkgs.haskellPackages.shellFor {

        packages = hpkgs: [
          # reuse the nixpkgs for this package
          hpkgs.distribution-nixpkgs
          # call our generated Nix expression manually
          hdeps
        ];

        # development tools we use
        nativeBuildInputs = with pkgs; [
          alejandra
          cabal-install
          cabal2nix
          haskell-language-server
          haskellPackages.doctest
          postgresql
        ];

        shellHook =
         ''
          source .env
         '';
      };
    });
}
