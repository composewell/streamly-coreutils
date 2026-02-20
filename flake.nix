{
  description = "Streamly Coreutils Development Environment";

  inputs = {
    basepkgs.url = "git+ssh://git@github.com/composewell/streamly-packages?rev=76420910d9c74e5fc1d92d680499c97e4f94e873";
    nixpkgs.follows = "basepkgs/nixpkgs";
    nixpkgs-darwin.follows = "basepkgs/nixpkgs-darwin";
  };

  outputs = { self, nixpkgs, nixpkgs-darwin, basepkgs }:
    basepkgs.nixpack.mkOutputs {
      inherit nixpkgs nixpkgs-darwin basepkgs;
      name = "streamly-coreutils";
      sources = basepkgs.nixpack.lib.localSource "streamly-coreutils" ./.;
      #packages = basepkgs.nixpack.lib.devPackage "streamly-coreutils";
      #sources = import ./sources.nix;
      packages = import ./packages.nix;
    };
}
