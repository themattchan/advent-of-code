{ mkDerivation, base, myutils, rtree, stdenv }:
mkDerivation {
  pname = "day3";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base myutils rtree ];
  license = stdenv.lib.licenses.bsd3;
}
