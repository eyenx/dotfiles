{
  config,
  lib,
  pkgs,
  ...
}:
let
  user = config.eyenx.user.name;
in
{
  config = lib.mkMerge [
    {
      #eyenx.core.zfs = lib.mkMerge [
      #  (lib.mkIf config.eyenx.persistence.enable {
      #    homeCacheLinks = [
      #      #   ".config"
      #      ".cache"
      #      # ".local"
      #      ".claude"
      #    ];
      # homeCacheFileLinks = [".claude.json"];
      #  })
      #];
    }
    {
      # environment.pathsToLink = [
      #  "/share/applications"
      #  "/share/xdg-desktop-portal"
      #];
      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        # Fix for file conflicts during darwin-rebuild/home-manager activation
        backupFileExtension = "backup";
        users = {
          "${user}" =
            { ... }:
            {
              # Common config
              imports = [
                #   inputs.catppuccin.homeModules.catppuccin
                #inputs.nix-colors.homeManagerModules.default
                #inputs.zen-browser.homeModules.twilight-official
              ];

              #colorScheme = inputs.nix-colors.colorSchemes.catppuccin-macchiato;

              home = {
                stateVersion = config.eyenx.stateVersion;
                username = config.eyenx.user.name;
                homeDirectory = config.eyenx.user.homeDirectory;
              };

              xdg.desktopEntries = {
                  wfica = {
                    name = "Citrix Receiver Engine";
                    type = "Application";
                    exec = "/home/eye/bin/wfica";
                    categories=["Application" "Network" "X-Red-Hat_Base" "X-SuSE-Core-Internet"];
                    mimeType = ["application/x-ica"];
                  };
              };
              dconf = {
                settings = {
                  "org/gnome/desktop/interface" = {
                    gtk-theme = "gruvbox";
                    color-scheme = "prefer-dark";
                    cursor-theme = "Openzone";
                  };
                };
              };

              gtk = {
                enable = true;
                theme = {
                  name = "gruvbox";
                };
                # TODO to fix
                cursorTheme = {
                  package = pkgs.openzone-cursors;
                  name = "Openzone";
                };
                iconTheme = {
                  package = pkgs.adwaita-icon-theme;
                  name = "Adwaita";
                };

                font = {
                  name = "CozetteVector";
                  size = 10;
                };
              };
              qt = {
                enable = true;
                platformTheme.name = "gruvbox";
                style = {
                  name = "gruvbox";
                };
              };
              programs.home-manager.enable = true;
              programs.zsh = {
                enable = true;
                history.size = 10000;
                envExtra = builtins.readFile "/home/eye/.myzshenv";
                initContent = builtins.readFile "/home/eye/.myzshrc";
              };
              programs.waybar.enable = true;
              programs.firefox = {
                enable = true;
              };
              programs.direnv = {
                enable = true;
                enableZshIntegration = true;
                nix-direnv.enable = true;
              };
              fonts.fontconfig = {
                enable = true;
                defaultFonts = {
                  monospace = ["Cozette" "Noto Color Emoji"];
                  sansSerif = ["Cozette" "Noto Color Emoji"];
                  serif = ["Cozette" "Noto Color Emoji"];
                  emoji = ["Cozette" "Noto Color Emoji"];
                };
              };
              services.swayidle =
                let
                  lock = "${pkgs.swaylock}/bin/swaylock -u  -c 282828";
                  display = status: "${pkgs.niri}/bin/niri msg action power-${status}-monitors";
                in
                {
                  enable = true;
                  timeouts = [
                    {
                      timeout = 295; # in seconds
                      command = "${pkgs.libnotify}/bin/notify-send 'Locking in 5 seconds' -t 5000";
                    }
                    {
                      timeout = 300;
                      command = lock;
                    }
                  ];
                  events = [
                    {
                      event = "before-sleep";
                      command = lock;
                    }
                  ];
                };

            };
        };
      };
    }
  ];
}
