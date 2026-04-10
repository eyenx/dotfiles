{ lib, config, ... }:
{
  options = {

    eyenx.font = lib.mkOption {
      type = lib.types.str;
      description = "Default font for the system.";
    };

    # System version option
    eyenx.stateVersion = lib.mkOption {
      type = lib.types.str;
      example = "25.05";
      description = "NixOS state version";
    };

    eyenx.timeZone = lib.mkOption {
      type = lib.types.str;
      default = "Europe/Zurich";
      description = "Time zone for the system.";
    };

    # Impermanence options
    eyenx.persistence = {
      enable = lib.mkEnableOption "Enable persistence/impermanence";

      dataPrefix = lib.mkOption {
        type = lib.types.str;
        default = "/persist";
        description = "Prefix for persistent data storage";
      };
    };

    # Stub for core namespace so that shared modules referencing
    # `eyenx.core.*` options are accepted when running on platforms
    # that do not import the Linux-specific ZFS module.
    #eyenx.core = lib.mkOption {
    #  type = lib.types.submodule { };
    #  default = { };
    #  description = "Namespace for Linux-only core settings. Empty on Darwin.";
    #};
  };

  config = {
    eyenx.font = "EnvyCodeR Nerd Font";
    eyenx.persistence.enable = true;
  };
}
