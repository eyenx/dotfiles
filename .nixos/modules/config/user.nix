{
  lib,
  config,
  pkgs,
  ...
}:
{
  options.eyenx.user = {
    name = lib.mkOption {
      type = lib.types.str;
      default = "eye";
      description = "Primary user name";
    };

    fullName = lib.mkOption {
      type = lib.types.str;
      default = "Toni";
      description = "User full name";
    };

    email = lib.mkOption {
      type = lib.types.str;
      default = "eye@eyenx.ch";
      description = "User email address";
    };

    gpgKey = lib.mkOption {
      type = lib.types.str;
      default = "32A63525D40A109B684936BC0796132F0077A5F8";
      description = "User GPG key";
    };

    homeDirectory = lib.mkOption {
      type = lib.types.str;
      default = "/home/${config.eyenx.user.name}";
      description = "User home directory";
    };

    devDir = lib.mkOption {
      type = lib.types.str;
      description = "Projects / Code directory";
      default = "dev";
    };

    shell = lib.mkOption {
      type = lib.types.str;
      default = "zsh";
      description = "Default shell";
    };

    editor = lib.mkOption {
      type = lib.types.str;
      default = "nvim";
      description = "Default editor";
    };
  };

  config = {
    users = {
      mutableUsers = config.eyenx.persistence.enable;
      # Base user configuration
      users.root = {
        hashedPasswordFile = config.sops.secrets."users/root".path;
      };
      users.${config.eyenx.user.name} = {
        home = config.eyenx.user.homeDirectory;
        isNormalUser = true;
        group = config.eyenx.user.name;
        hashedPasswordFile = config.sops.secrets."users/${config.eyenx.user.name}".path;
        shell = pkgs.zsh;
        extraGroups = [
          "networkmanager"
          "systemd-journal"
          "systemd-resolve"
          "wheel"
          "dialout"
          "seat"
          "uinput"
        ];
      };
      groups.${config.eyenx.user.name} = { };
    };
  };
}
