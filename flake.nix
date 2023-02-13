{
  description = "nix flake support for paganini-hs";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let

        name = "pagnaini-hs";
        compiler = "ghc925";
         
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
            allowBroken = true;
            allowUnsupportedSystem = true;
          };
        };

        optas = p: with p;
          (
            buildPythonPackage rec {
              pname = "optas";
              version = "1.0.3";
              src = fetchPypi {
                inherit pname version;
                sha256 = "sha256-92aaW9ZxvRy/dfYhw7IS+lfDAP2UuBuJhNDNTW7Xkzc=";
              };
              doCheck = false;
            }
          );

        paganini = p: with p;
          (
            buildPythonPackage rec {
              pname = "paganini";
              version = "1.5.0";
              src = fetchPypi {
                inherit pname version;
                sha256 = "sha256-hsDqONhBPQlgQHKBh5tdp/pzoXynUz7ztXXILrGgslo=";
              };
              doCheck = false;
              propagatedBuildInputs = [
                pkgs.python3Packages.numpy
                pkgs.python3Packages.sympy
                pkgs.python3Packages.scipy
                pkgs.python3Packages.networkx
                pkgs.python3Packages.cvxpy
                (optas p)
              ];
            }
          );

        pythonPackages = p: with p; [
          (optas p)
          (paganini p)
        ];
       
        haskellPackages = pkgs.haskell.packages.${compiler}.override {
          overrides = self: super: {
            "${name}" = self.callPackage ./. {
              paganini = (pkgs.python3.withPackages pythonPackages);
            };
          };
        };
        
        devShell = haskellPackages.shellFor {
          withHoogle = true; # Provides docs, optional. 
          packages = p: [
            p."${name}"
          ];

          buildInputs = with pkgs; [
            haskellPackages.stack
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.fourmolu
            (pkgs.python3.withPackages pythonPackages)
          ];
        };

        defaultPackage = haskellPackages."${name}";
        derive = import ./.;
        
      in
        {
          inherit derive defaultPackage devShell;
        });
}
