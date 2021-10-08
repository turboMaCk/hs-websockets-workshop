{ pkgs ? import <nixpkgs> {} }:
let
  appEnv = (import ./default.nix { inherit pkgs; }).env;
in with pkgs;
mkShell {
  inputsFrom = [ appEnv ];
  buildInputs = [];
}
