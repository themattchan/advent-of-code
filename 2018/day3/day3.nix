{ mkDerivation, base, myutils, data-r-tree, containers, stdenv }:
mkDerivation {
  pname = "day3";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base myutils data-r-tree containers];
  license = stdenv.lib.licenses.bsd3;
}
