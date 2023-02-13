{ mkDerivation, base, BinderAnn, containers, mtl, multiset, process
, lib, tasty, tasty-hunit
# python package needed at test and run time, undeclared in .cabal file
, paganini
}:
mkDerivation {
  pname = "paganini-hs";
  version = "0.5.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers mtl multiset process tasty tasty-hunit
  ];
  testHaskellDepends = [
    base
    tasty
    tasty-hunit
    paganini
  ];
  homepage = "https://github.com/maciej-bendkowski/paganini-hs#readme";
  license = lib.licenses.bsd3;
}
