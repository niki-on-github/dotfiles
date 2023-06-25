# 1. `git add flake.nix shell.nix`
# 2: `nix develop --impure`
{
  description = "detectron2";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system}; in
        {
          devShells.default = import ./shell.nix { inherit pkgs; };
          apps.default = {
            type = "app";
            program = "${pkgs.tts}/bin/tts";
          };
        }
      );
}
