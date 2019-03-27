let
  nixpkgs = fetchGit {
    url = git://github.com/NixOS/nixpkgs-channels;
    ref = "nixos-19.03";
  };

  config = {
	packageOverrides = pkgs: rec {
	  haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
           project
	   = haskellPackagesNew.callCabal2nix "discourse-tui" ../discourse-tui {};
	  
	   validity
	   = haskellPackages.callHackage "validity" "0.8.0.0" {};

	   cursor
	   = haskellPackages.callHackage "cursor" "0.0.0.1" {};
	    };
	  };
	};
  };

  pkgs = import nixpkgs {inherit config;};
in
  {
   project = pkgs.haskellPackages.project;
  }
