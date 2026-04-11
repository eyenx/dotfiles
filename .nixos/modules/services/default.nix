{ pkgs, ... }:
let
  tuigreet = "${pkgs.tuigreet}/bin/tuigreet";
  niri = "${pkgs.niri}/bin/niri-session";
in
{
  # bolt
  services.hardware.bolt.enable = true;

  # resolved
  services.resolved = {
    enable = true;
    domains = [ "~." ];
  };

  # keyd
  services.keyd = {
    enable = true;
    keyboards = {
      default = {
        ids = [ "*" ];
        settings = {
          main = {
            capslock = "layer(mod)";
          };
          mod = {
            h = "left";
            j = "down";
            l = "right";
            k = "up";
            p = "delete";
            b = "home";
            n = "end";
          };
        };
      };
    };
  };
  # keyd group and fix for
  # https://github.com/NixOS/nixpkgs/issues/290161
  users.groups.keyd = { };
  systemd.services.keyd.serviceConfig.CapabilityBoundingSet = [
    "CAP_SETGID"
  ];

  # greetd / tuigreet
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${tuigreet} --time --remember --remember-session --cmd ${niri}";
        user = "greeter";
      };
    };
  };
  systemd.services.greetd.serviceConfig = {
    Type = "idle";
    StandardInput = "tty";
    StandardOutput = "tty";
    StandardError = "journal"; # Without this errors will spam on screen
    # Without these bootlogs will spam on screen
    TTYReset = true;
    TTYVHangup = true;
    TTYVTDisallocate = true;
  };

  # cups
  services.printing = {
    enable = true;
    drivers = [ pkgs.hplipWithPlugin ];
  };

  # pipewire
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };

  # gnome-keyring
  services.gnome.gnome-keyring.enable = true;

  # openssh
  services.openssh.enable = true;

  # uinput
  hardware.uinput.enable = true;

  # bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    settings = {
      General = {
        Experimental = true;
      };
      Policy = {
        AutoEnable = true;
      };
    };
  };
  services.blueman.enable = true;

  # netbird
  services.netbird.enable = true;

  # user services & timers
  # TODO make this conifgurable through user.nix
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
}
