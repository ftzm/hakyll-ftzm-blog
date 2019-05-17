{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "ftzm-blog";
  inherit ghc;
  buildInputs = with pkgs; [ zlib unzip sass inotify-tools];
  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}