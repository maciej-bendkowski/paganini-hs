{ mkDerivation, base, BinderAnn, containers, mtl, multiset, process
, stdenv, tasty, tasty-hunit
# python package needed at test and run time, undeclared in .cabal file
, paganini 
}:
mkDerivation {
  pname = "paganini-hs";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base BinderAnn containers mtl multiset process tasty tasty-hunit
  ];
  testHaskellDepends = [
    base
    BinderAnn
    tasty
    tasty-hunit
    paganini
  ];
  homepage = "https://github.com/maciej-bendkowski/paganini-hs#readme";
  license = stdenv.lib.licenses.bsd3;
}
