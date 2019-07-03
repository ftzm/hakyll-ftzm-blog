{ pkgs ? import <nixpkgs> { }, mkDerivation ? pkgs.stdenv.mkDerivation, version ? "latest" }:

let
  inherit (import <nixpkgs> { }) fetchFromGitHub;
  # ----------------------------------------------------------------------------
  # Import stable nixpkgs as buildImage was broken on unstable
  # at time of writing.
  nixpkgs_19_03 = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "2516c454c35344d551420fb74541371c6bfcc5e9";
    sha256 = "1fsxwd3x5ag1ihgprrlgk06s5ja7rkk5a5s04qmrp32784iylkpn";
  };
  pkgs_19_03 = import nixpkgs_19_03 { };

  # ----------------------------------------------------------------------------
  # Docker
  repo = "ftzm";
  name = "blog";

in rec {
  generator = pkgs.stdenv.mkDerivation {
    name = "ftzm-blog-generator";
    src = ./src;
    phases = "unpackPhase buildPhase";
    buildInputs = [
      (pkgs.haskellPackages.ghcWithPackages (p:
      with p; [
        hakyll
        blaze-html
        containers
        unordered-containers
        yaml
        text
        vector
        split
        pandoc
      ]))
    ];
    buildPhase = ''
      mkdir -p $out/bin
      ghc -O2 -dynamic --make site.hs -o $out/bin/generate-site
    '';
  };
  files = mkDerivation {
    name = "ftzm-blog";
    src = ./src;
    phases = "unpackPhase buildPhase";
    version = "0.1";
    buildInputs = [ generator ];
    buildPhase = ''
      mkdir $out

      export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
      export LANG=en_US.UTF-8
      generate-site build

      cp -r _site/* $out
    '';
  };
  # ----------------------------------------------------------------------------
  # Nginx
  nginxPort = "80";
  nginxWebRoot = files;
  nginxConf = pkgs.writeText "nginx.conf" ''
          user nginx nginx;
          daemon off;
          error_log /dev/stdout info;
          pid /dev/null;
          events {}
          http {
            include    ${pkgs.nginx}/conf/mime.types;
            access_log /dev/stdout;
            root ${nginxWebRoot};
            types {
            	  text/html	html htm shtml;
                text/css	css;
            }
            server {
              listen ${nginxPort};
              location / {
                location = /index.html {return 301 $scheme://$http_host/articles;}

                # hide .html ending
                if ($request_uri ~ ^/(.*)\.html$) {
		                return 302 $scheme://$http_host/$1;
                }
                try_files $uri $uri.html $uri/ =404;
              }
	            error_page 404 /404.html;
            }
          }
      '';
  # ----------------------------------------------------------------------------
  # image
  image = pkgs_19_03.dockerTools.buildImage {
    name = "${repo}/${name}";
    tag = version;
    contents = pkgs.nginx;

    runAsRoot = ''
      #!${pkgs.stdenv.shell}
      ${pkgs.dockerTools.shadowSetup}
      groupadd --system nginx
      useradd --system --gid nginx nginx
    '';

    config = {
      Cmd = [ "nginx" "-c" nginxConf ];
      ExposedPorts = { "${nginxPort}/tcp" = { }; };
    };
  };
}
