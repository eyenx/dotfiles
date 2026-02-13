# Shared System Configuration

{
  config,
  pkgs,
  ...
}:
{
  config = {
    # Allow unfree because we're not free :(
    nixpkgs.config = {
      allowUnfree = true;
    };

    # timezone
    time.timeZone = config.eyenx.timeZone;

    # zsh everywhere
    programs.zsh.enable = true;
    programs.nix-ld.enable = true;
    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    programs.niri.enable = true;

    environment.shells = with pkgs; [ zsh ];
    users.defaultUserShell = pkgs.zsh;

    # default editor
    environment.variables.EDITOR = config.eyenx.user.editor;

    # fonts
    fonts = {
      packages = with pkgs; [
        material-design-icons
        adwaita-fonts
        cozette
        font-awesome
        nerd-fonts.zed-mono
        noto-fonts-emoji-blob-bin
        nerd-fonts.symbols-only
        nerd-fonts._0xproto
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
