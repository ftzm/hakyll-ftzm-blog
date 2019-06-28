{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

let dhall_latest =  import <dhall> {};
in

pkgs.haskell.lib.buildStackProject {
  name = "ftzm-blog";
  inherit ghc;
  buildInputs = with pkgs; [
    zlib
    unzip
    sass
    inotify-tools
    dhall_latest.linux-dhall-json
  ];
  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
