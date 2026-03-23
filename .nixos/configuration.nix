# Edit this configuration file to define what should be installed on your system. Help is available in the configuration.nix(5) man page, on https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{
  config,
  lib,
  pkgs,
  ...
}:
{

  # TODO gomuks workaround
  nixpkgs.config.permittedInsecurePackages = [
    "olm-3.2.16"
  ];

  # TODO matterhorn binary
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [
    # matterhorn
    gmp
    libtinfo
  ];

  nixpkgs.config.allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "claude-code"
      "slack"
    ];

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;

  security.polkit.enable = true;
  security.pam.services.swaylock-plugin = { };

  # user system services
  systemd.user.services = {
    fetchmail = {
      enable = true;
      description = "Fetch my mail with offlineimap";
      after = [ "network.target" ];
      path = [
        pkgs.bash
        pkgs.offlineimap
        pkgs.bc
        pkgs.notmuch
        pkgs.lbdb
        pkgs.oama
        pkgs.gopass
        pkgs.procps
        pkgs.gawk
      ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "/home/eye/bin/fetchmail.sh";
        TimeoutStartSec = 300;
      };
    };
    ydotoold = {
      enable = true;
      description = "An auto-input utility for wayland";
      serviceConfig = {
        Type = "simple";
        ExecStart = "/run/current-system/sw/bin/ydotoold --socket-path /run/user/1000/.ydotool_socket";
      };

      wantedBy = [ "default.target" ];
    };
  };

  # user system timers
  systemd.user.timers = {
    fetchmail = {
      enable = true;
      description = "Fetch my mail with offlineimap";
      after = [ "network.target" ];
      timerConfig = {
        OnCalendar = "*:0/15"; # every 15 minutes
        Persistent = true;
      };
    };
  };

  # persistence
  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/var/log"
      "/var/lib/bluetooth"
      "/var/lib/nixos"
      "/var/lib/systemd/coredump"
      "/var/lib/systemd/timers"
      "/etc/NetworkManager/system-connections"
      {
        directory = "/var/lib/colord";
        user = "colord";
        group = "colord";
        mode = "u=rwx,g=rx,o=";
      }
    ];
    files = [
      "/etc/machine-id"
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_ed25519_key.pub"
    ];
  };

}
