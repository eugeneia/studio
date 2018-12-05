# snabb nix library API:
#
#   snabb.processDirectory <path>
#   snabb.processTarball <url>
#     Process a Snabb shm folder for presentation.

{ pkgs ? import ../../nix/pkgs.nix {} }:

with pkgs; with stdenv;

rec {
  processDirectory = dir: runCommand "snabb-directory" { inherit dir; } ''
    cp --no-preserve=mode -r $dir $out
    cd $out
  '';
  processTarball = url: runCommand "snabb-tarball" { inherit url; } ''
    mkdir -p $out && cd $out
    ${curl}/bin/curl "$url" | tar -x -J -k
  '';

}
