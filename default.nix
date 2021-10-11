{ pkgs ? import <nixpkgs> {} }:
with pkgs;
(haskellPackages.callPackage ./app.nix {}).overrideDerivation(super: {
  postInstall = super.postPatch + ''
    mkdir -p $out/var
    cp index.html $out/var/index.html
  '';
})
