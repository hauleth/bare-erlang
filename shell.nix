{ pkgs ? import <nixpkgs> {} }:

with pkgs.beam.packages.erlangR22;

pkgs.mkShell {
  buildInputs = [
    rebar3
    erlang
  ];
}
