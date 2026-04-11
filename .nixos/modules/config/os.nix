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
  };

  config = {
    eyenx.font = "Mononoki Nerd Font";
    eyenx.persistence.enable = true;
  };
}
