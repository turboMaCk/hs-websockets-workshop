{ mkDerivation, base, lib, websockets }:
mkDerivation {
  pname = "hs-websockets-workshop";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base websockets ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
