{ mkDerivation, base, myutils, data-r-tree, stdenv }:
mkDerivation {
  pname = "day3";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base myutils data-r-tree ];
  license = stdenv.lib.licenses.bsd3;
}
