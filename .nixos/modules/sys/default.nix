# Shared System Configuration

{
  config,
  pkgs,
  lib,
  ...
}:
{
  config = {
    # Allow unfree because we're not free :(
    nixpkgs.config = {
      allowUnfree = true;
      # TODO gomuks workaround
      permittedInsecurePackages = [
        "olm-3.2.16"
        "electron-39.8.10"
      ];
      allowUnfreePredicate =
        pkg:
        builtins.elem (lib.getName pkg) [
          "claude-code"
          "slack"
        ];
    };

    # timezone
    time.timeZone = config.eyenx.timeZone;

    # impersistence
    # TODO make this optionals depending on config/os.nix
    environment.persistence."/persist" = {
      hideMounts = true;
      directories = [
        "/var/log"
        "/var/lib/bluetooth"
        "/var/lib/nixos"
        "/var/lib/libvirt"
        "/var/lib/systemd/coredump"
        "/var/lib/systemd/timers"
        "/etc/NetworkManager/system-connections"
        "/var/ossec"
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

    # locale
    i18n.defaultLocale = config.eyenx.user.locale;

    # polkit
    security = {
      polkit.enable = true;
      pam.services.swaylock-plugin = { };
    };

    # zsh everywhere
    programs.zsh.enable = true;
    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    programs.niri.enable = true;

    # nix-ld
    # TODO matterhorn binary
    programs.nix-ld.enable = true;
    programs.nix-ld.libraries = with pkgs; [
      # matterhorn
      gmp
      libtinfo
    ];

    environment.shells = with pkgs; [ zsh ];
    users.defaultUserShell = pkgs.zsh;

    # default editor
    environment.variables.EDITOR = config.eyenx.user.editor;

    # fonts
    fonts = {
      packages = with pkgs; [
        adwaita-fonts
        font-awesome
        material-design-icons
        noto-fonts-emoji-blob-bin
        nerd-fonts.symbols-only
        nerd-fonts.mononoki
      ];
    };

    # nix config
    nix = {
      enable = true;
      package = pkgs.nix;
      settings = {
        trusted-users = [ config.eyenx.user.name ];
        experimental-features = [
          "nix-command"
          "flakes"
        ];
        warn-dirty = false;
        auto-optimise-store = false;
      };

      # garbage collection
      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 14d";
      };

      optimise = {
        automatic = true;
        dates = "weekly";
      };
    };
  };
}
