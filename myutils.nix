{ mkDerivation, base, containers, stdenv }:
mkDerivation {
  pname = "myutils";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers ];
  license = stdenv.lib.licenses.bsd3;
}
