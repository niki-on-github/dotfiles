{
  description = "Wine Apps NixOS/Nix Flakes";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";
  outputs = { self, nixpkgs }: {

    lib.x86_64-linux = let
      pkgs = import "${nixpkgs}" {
        system = "x86_64-linux";
        config.allowUnfree = true;
      };

      callPackage = pkgs.callPackage;
    in {
      mkWindowsApp = callPackage ./lib/mkwindowsapp {
        makeBinPath = pkgs.lib.makeBinPath;
      };
      copyDesktopIcons = pkgs.makeSetupHook { name = "copyDesktopIcons"; } ./hooks/copy-desktop-icons.sh;
      makeDesktopIcon = callPackage ./lib/makeDesktopIcon.nix {};
      genericBinWrapper = callPackage ./lib/generic-bin-wrapper.nix { };
    };

    packages.x86_64-linux = let
      pkgs = import "${nixpkgs}" {
        system = "x86_64-linux";
        config.allowUnfree = true;
      };

      callPackage = pkgs.callPackage;
      lib = self.lib.x86_64-linux;
      in {
        notepad-plus-plus = callPackage ./pkgs/notepad++.nix {
          mkWindowsApp = lib.mkWindowsApp;
          wine = pkgs.wineWowPackages.full;
          wineArch = "win64";
          copyDesktopIcons = lib.copyDesktopIcons;
          makeDesktopIcon = lib.makeDesktopIcon;
        };

        mkwindowsapp-tools = callPackage ./lib/mkwindowsapp-tools { wrapProgram = pkgs.wrapProgram; };

    };
  };
}
