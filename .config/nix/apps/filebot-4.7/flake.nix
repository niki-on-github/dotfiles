# usage: `nix run`
{
  description = "filebot-4.7.9";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
          };
          overlays = [ (final: prev: {
            filebot = prev.filebot.overrideAttrs (previousAttrs: rec {
              version = "4.7.9";
              src = prev.fetchurl {
                url = "https://f002.backblazeb2.com/file/AUR-Store/filebot47/FileBot_4.7.9-portable.tar.xz";
                sha256 = "sha256-JTG8Z9NSKx2wWHDaK3L/9YDr+llpZV51OC/+LI3saGc=";
              };
            installPhase = ''
                mkdir -p $out/opt $out/bin
                # Since FileBot has dependencies on relative paths between files, all required files are copied to the same location as is.
                cp -r * $out/opt/
                # Filebot writes to $APP_DATA, which fails due to read-only filesystem. Using current user .local directory instead.
                substituteInPlace $out/opt/filebot.sh \
                  --replace 'APP_DATA="$APP_ROOT/data"' 'APP_DATA=''${XDG_DATA_HOME:-$HOME/.local/share}/filebot/data' \
                  --replace '$FILEBOT_HOME/data/.license' '$APP_DATA/.license'
                wrapProgram $out/opt/filebot.sh \
                  --prefix PATH : ${prev.lib.makeBinPath [ prev.pkgs.openjdk11 ]}
                # Expose the binary in bin to make runnable.
                ln -s $out/opt/filebot.sh $out/bin/filebot
              '';
            });
          }) ];
        };
      in
      {
        defaultPackage = pkgs.filebot;
        overlay = pkgs.overlays;
      }
    );
}
