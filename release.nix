let
  nixpkgs = fetchGit {
    url = git://github.com/NixOS/nixpkgs-channels;
    ref = "nixos-18.09";
  };

  config = {
	packageOverrides = pkgs: rec {
	  haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
           project
	   = haskellPackagesNew.callCabal2nix "discourse-tui" ../discourse-tui {};
	    };
	  };
	};
  };

  pkgs = import nixpkgs {inherit config;};
in
  {
   project = pkgs.haskellPackages.project;
  }
