# CAUTION! a spelling mistake in arg string is ignored silently.
#
# To use ghc-8.10.7
# nix-shell --argstr compiler "ghc8107"

{
  nixpkgs ?
    import (builtins.fetchTarball
      https://github.com/NixOS/nixpkgs/archive/refs/tags/22.05.tar.gz)
        {}
, compiler ? "ghc922"
}:
let
    utils =
      let src = fetchGit {
            url = "git@github.com:composewell/composewell-haskell.git";
            ref = "master";
          }; in (import "${src}/utils.nix") { inherit nixpkgs; };


    haskellPackages =
      let src = fetchGit {
            url = "git@github.com:composewell/composewell-haskell.git";
            ref = "master";
          }; in (import "${src}/haskellPackages.nix")
            { inherit nixpkgs;
              inherit compiler; };

    mkHaskellPackages = inShell:
      haskellPackages.override (old: {
        overrides =
          nixpkgs.lib.composeExtensions
            (old.overrides or (_: _: {}))
            (with nixpkgs.haskell.lib; self: super: {
                  streamly-coreutils =
                      utils.local super "streamly-coreutils" ./. "--benchmark" inShell;
                  streamly = utils.composewell super
                      "streamly"
                      "9596aea3ab33d3c2fa513247fcfe8604bd7ff454";
                  streamly-core = utils.composewellDir super
                      "streamly"
                      "9596aea3ab33d3c2fa513247fcfe8604bd7ff454"
                      "/core";
                  streamly-process = utils.composewell super
                      "streamly-process"
                      "d80b860d9d8ea98e4f7f63390442b3155c34dd08";
            });
      });

    shellDrv = mkHaskellPackages true;

    shell = utils.mkShell shellDrv (p: [p.streamly-coreutils]) true;

in if nixpkgs.lib.inNixShell
   then shell
   else (mkHaskellPackages false).streamly-coreutils
