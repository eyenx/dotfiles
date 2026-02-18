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
}
