{ inputs, pkgs, ... }:
{
  additions = final: prev: import ../pkgs { pkgs = final; };
  unstable-packages = final: prev: {
    unstable = import inputs.nixpkgs-unstable { system = prev.stdenv.hostPlatform.system; };
  };
  niri-scratchpad = final: prev: {
    niri-scratchpad = inputs.niri-scratchpad.packages.${pkgs.system}.default;
  };

  force-latest =
    final: prev:
    let
      main = import inputs.nixpkgs-main {
        system = prev.stdenv.hostPlatform.system;
        overlays = [ ];
      };
    in
    {
      nix-init = main.nix-init;
      nurl = main.nurl;
      nix = main.nix;
    };
}
