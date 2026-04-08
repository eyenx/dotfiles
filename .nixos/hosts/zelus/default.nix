{ ... }:
let
  nixVersion = "25.11";
in
{
  imports = [
    ./hardware.nix
  ];

  networking = {
    hostName = "zelus";
    networkmanager = {
      enable = true;
    };
  };

  eyenx = {
    stateVersion = nixVersion;
    #gpg.enable = false;
    persistence.enable = true;
    #core = {};
    devpkgs = {
      enable = true;
      tools = {
        ai = true;
        c = true;
        nix = true;
        go = true;
        k8s = true;
        iac = true;
        python = true;
        networking = true;
        security = true;
        cloud = true;
        dev = true;
      };
    };
    # dev = {};
    #graphical = {
    #  enable = true;
    #  laptop = true;
    #  niri = {
    #    enable = true;
    #  };
    #  xdg = {
    #    enable = true;
    #  };
    #};
  };

  system.stateVersion = nixVersion;
}
