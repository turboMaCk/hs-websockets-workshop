{ mkDerivation, aeson, base, hashable, lib, random, stm, text
, unordered-containers, websockets
}:
mkDerivation {
  pname = "hs-websockets-workshop";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base hashable random stm text unordered-containers websockets
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
