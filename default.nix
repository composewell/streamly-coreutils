# CAUTION! a spelling mistake in arg string is ignored silently.
#
# To use ghc-8.6.5
# nix-shell --argstr compiler "ghc865"

{
  nixpkgs ?
    import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/21.11.tar.gz)
        {}
, compiler ? "default"
, c2nix ? "" # cabal2nix CLI options
# TODO
#, sources ? [] # e.g. [./. ./benchmark]
#, hdeps ? [] # e.g. [time, mtl]
#, deps ? [] # e.g. [SDL2]
}:
let haskellPackages =
        if compiler == "default"
        then nixpkgs.haskellPackages
        else nixpkgs.haskell.packages.${compiler};

    # we can possibly avoid adding our package to HaskellPackages like
    # in the case of nix-shell for a single package?
    mkPackage = super: pkg: path: opts: inShell:
                let orig = super.callCabal2nixWithOptions pkg path opts {};
                 in if inShell
                    # Avoid copying the source directory to nix store by using
                    # src = null.
                    then orig.overrideAttrs (oldAttrs: { src = null; })
                    else orig;

    flags = "--benchmark --flag fusion-plugin" + " " + c2nix;

    mkHaskellPackages = inShell:
        haskellPackages.override {
            overrides = self: super:
                with nixpkgs.haskell.lib;
                {
                    streamly-coreutils = mkPackage super "streamly-coreutils" ./. flags inShell;

                    streamly =
                      nixpkgs.haskell.lib.overrideCabal
                        #(super.callHackageDirect
                        #  { pkg = "streamly";
                        #    ver = "0.8.2";
                        #    sha256 = "sha256-CjFq9SCdbgLZa7NqOE4OtC8OaFg4vK8VmIDjGU5rGko=";
                        #  } {})
                        (let src = fetchGit {
                            url = "git@github.com:composewell/streamly.git";
                            rev = "efa19570f8ef93650faab12b400043fde2c88390";
                        }; in super.callCabal2nix "streamly" src {})
                        (old:
                          { librarySystemDepends =
                              if builtins.currentSystem == "x86_64-darwin"
                              then [nixpkgs.darwin.apple_sdk.frameworks.Cocoa]
                              else [];
                            enableLibraryProfiling = false;
                            doHaddock = false;
                          });

                    streamly-core =
                      nixpkgs.haskell.lib.overrideCabal
                        (let src = fetchGit {
                            url = "git@github.com:composewell/streamly.git";
                            rev = "efa19570f8ef93650faab12b400043fde2c88390";
                        }; in super.callCabal2nix "streamly-core" "${src}/core" {})
                        (old:
                          { librarySystemDepends =
                              if builtins.currentSystem == "x86_64-darwin"
                              then [nixpkgs.darwin.apple_sdk.frameworks.Cocoa]
                              else [];
                            enableLibraryProfiling = false;
                            doHaddock = false;
                          });

                    unicode-data =
                      super.callHackageDirect
                        { pkg = "unicode-data";
                          ver = "0.3.0";
                          sha256 = "sha256-3R8ZmLoN/oWU0Mr/V4o/90NqiWaE8fprVULgh8/s/Uc=";
                        } {};

                    streamly-process =
                      nixpkgs.haskell.lib.overrideCabal
                        (let src = fetchGit {
                            url = "git@github.com:composewell/streamly-process.git";
                            rev = "e310c8a9c6e61515373a2f5f1fb82f6e15fe452b";
                            ref = "master";
                        }; in super.callCabal2nix "streamly-process" src {})
                      #  (super.callHackageDirect
                      #    { pkg = "streamly-process";
                      #      ver = "0.2.0.1";
                      #      sha256 = "sha256-no/U5aWkZJXaA6HN6H78iZiYu3kaR7i2Ouu8oewAN2o=";
                      #    } {})
                          (old:
                            { enableLibraryProfiling = false;
                              doHaddock = false;
                              #doCheck = false;
                            });

                };
        };

    drv = mkHaskellPackages true;

    # A fake package to add some additional deps to the shell env
    additionalDeps = drv.mkDerivation rec {
              version = "0.1";
              pname   = "streamly-coreutils-additional";
              license = "BSD-3-Clause";

              executableHaskellDepends = with drv; [
                unicode-data
              ];
            };

    shell = drv.shellFor {
        packages = p:
          [ p.streamly-coreutils
            additionalDeps
          ];
        doBenchmark = true;
        # Use a better prompt
        shellHook = ''
          export CABAL_DIR="$(pwd)/.cabal.nix"
          if test -n "$PS_SHELL"
          then
            export PS1="$PS_SHELL\[$bldred\](nix)\[$txtrst\] "
          fi
        '';
    };
in if nixpkgs.lib.inNixShell
   then shell
   else (mkHaskellPackages false).streamly-coreutils
