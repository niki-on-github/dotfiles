{ pkgs ? import <nixpkgs> {
  config = {
    allowUnfree = true;
    cudaSupport = true;
  };
}
}:
  pkgs.mkShell {
    nativeBuildInputs = with pkgs; [
      tts
      arcanPackages.espeak
    ];

}
