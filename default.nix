{ mkDerivation, base, stdenv, containers, TypeCompose, QuickCheck }:
mkDerivation {
  pname = "dddd";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers TypeCompose QuickCheck ];
  license = stdenv.lib.licenses.bsd3;
}
