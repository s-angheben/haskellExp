{ mkDerivation, base, lib }:
mkDerivation {
  pname = "monad";
  version = "0.1.0.0";
  sha256 = "monadtirocinio";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
