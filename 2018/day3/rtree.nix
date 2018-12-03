{ mkDerivation, base, binary, containers, deepseq, HUnit
, QuickCheck, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "data-r-tree";
  version = "0.0.5.0";
  sha256 = "1zgwm020zxfhb70llch4y075rd6klwwnv9yn8mpgh0rm0ib7jvyy";
  libraryHaskellDepends = [ base binary deepseq ];
  testHaskellDepends = [
    base binary containers HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2
  ];
  homepage = "https://github.com/sebastian-philipp/r-tree";
  description = "R-Tree is a spatial data structure similar to Quadtrees or B-Trees";
  license = stdenv.lib.licenses.mit;
}
