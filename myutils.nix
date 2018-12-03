{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "myutils";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
