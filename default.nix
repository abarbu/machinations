let
  sources = import nix/sources.nix {};
  haskell-nix = (import sources."haskell.nix" {});
  nixpkgs = haskell-nix.pkgs;
  gitignore = (import sources."gitignore.nix" {
    inherit (nixpkgs) lib;
  }).gitignoreSource;

  src = nixpkgs.lib.cleanSourceWith {
    name = "machinations";
    src = gitignore ./.;
  };
in
nixpkgs.haskell-nix.stackProject {
  inherit src;
  modules = [({pkgs, ...}: {
    packages.machinations.components.library.build-tools = [ pkgs.buildPackages.graphviz ];
    doHaddock = false;
  })];
}
