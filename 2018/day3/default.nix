{ compiler ? "ghc822" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =  pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
             overrides = haskellPackagesNew: haskellPackagesOld: rec {
             # this does not work in 8.4+ because it does not define a semigroup instance
               rtree =
               pkgs.haskell.lib.dontHaddock (
               pkgs.haskell.lib.dontCheck (
               haskellPackagesNew.callPackage ./rtree.nix { }));

               myutils =
                 haskellPackagesNew.callPackage ../../myutils.nix { };

               day3 =
                 haskellPackagesNew.callPackage ./day3.nix { };
               };
             };
           };
         };
       };

  pkgs = import <nixpkgs> { inherit config; };

in
{ day3 = pkgs.haskellPackages.${compiler}.day3;
}
