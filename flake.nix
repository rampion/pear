# Adapted from https://github.com/lf-/flake-templates/blob/main/haskell/flake.nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig.allow-import-from-derivation = true; # cabal2nix uses IFD
  
  description = "The pear package for haskell, describing and defining the PearTree data structure";

  outputs = { self, nixpkgs, flake-utils }:
    let 
      package-name = "pear";

      ghc-version = "ghc963"; 

      getBuildInputs = pkgs: haskell-packages: [
        haskell-packages.ghcid
        haskell-packages.cabal-install
        haskell-packages.pandoc
        pkgs.hlint
      ];

      withHoogle = true;
    in
    flake-utils.lib.eachDefaultSystem (system:
    let 
      pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
            config.allowBroken = true;
          };
      system-packages = self.packages.${system};
      haskell-packages = pkgs.haskell.packages.${ghc-version};
    in
    {
      packages = {
        ${package-name} = haskell-packages.${package-name};
        default = haskell-packages.${package-name};
      };

      checks = {
        ${package-name} = system-packages.${package-name};
      };

      devShells.default = haskell-packages.shellFor {
        packages = p: [ system-packages.${package-name} ];
        inherit withHoogle;
        buildInputs = getBuildInputs pkgs haskell-packages;
      };
    }) // {
      overlays = {
        default = final: prev: {
          haskell = prev.haskell // {
            packages = prev.haskell.packages // {
              ${ghc-version} = prev.haskell.packages.${ghc-version}.override (oldArgs: {
                overrides = prev.lib.composeExtensions 
                  (oldArgs.overrides or (_: _: { }))
                  (hfinal: hprev: { ${package-name} = hprev.callCabal2nix package-name ./. { }; });
              });
            };
          };
        };
      };
    };
}
