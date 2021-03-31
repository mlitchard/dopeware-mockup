# default.nix
{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
#   useWarp = true;
  packages = {
    common = ./common;
  };

  shells = {
    ghc = ["frontend"];
    ghcjs = ["frontend"];
  };
})
