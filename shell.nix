let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      devsurf = self.callPackage ./. {};
       };
       };

       in pkgs.lib.overrideDerivation haskellPackages.devsurf (attrs: {
       buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
       })
