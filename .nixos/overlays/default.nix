{ inputs, ... }:
{
  additions = final: prev: import ../pkgs { pkgs = final; };
  unstable-packages = final: prev: {
    unstable = import inputs.nixpkgs-unstable { system = final.system; };
  };

  force-latest =
    final: prev:
    let
      main = import inputs.nixpkgs-main {
        system = final.system;
        overlays = [ ];
      };
    in
    {
      nix-init = main.nix-init;
      nurl = main.nurl;
      nix = main.nix;
    };
}
