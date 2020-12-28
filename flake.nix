{
  description = "nix flake support for paganini-hs";

  inputs.nixpkgs.url = "github:NixOs/nixpkgs/nixpkgs-unstable"; 
  
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.mach-nix.url = "github:DavHau/mach-nix";

  outputs = { self, nixpkgs, flake-utils, mach-nix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let

        name = "pagnaini-hs";
        
        # paganini-hs dependency BinderAnn won't compile with later ghc versions 
        compiler = "ghc865";  
         
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
            allowBroken = true;
            allowUnsupportedSystem = true;
          };
          # overlays = [] # Add or tweak non-Haskell packages here.
        };

        mylib = mach-nix.lib.${system}; # adds mkPython, mkPythonShell, etc. 

        paganini-custom = mylib.mkPython {
          requirements = ''
            paganini==1.3.3
            cvxpy>=1.1.7 
            scs==2.1.1-2 # scs 2.1.2 broken 
          '';
        };
       
        haskellPackages = pkgs.haskell.packages.${compiler}.override {
          overrides = self: super: {
            #"${name}" = self.callCabal2nix name ./. {};
            "${name}" = self.callPackage ./. { paganini = paganini-custom; };
            # Override other Haskell packages as needed here.
          };
        };
        
        devShell = haskellPackages.shellFor {
          withHoogle = true; # Provides docs, optional. 
          packages = p: [
            p."${name}"
            # Add other Haskell packages below if you just want a Haskell hacking env.
            # p.lens
          ]; 
          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.ghcid
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
            haskellPackages.cabal2nix
            paganini-custom
          ];
        };

        defaultPackage = haskellPackages."${name}";
        derive = import ./.;
        
      in
        {
          inherit derive defaultPackage devShell;
        });
}
