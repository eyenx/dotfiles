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
                packages = [
                  pkgs.age
                  pkgs.bc
                  pkgs.awscli2
                  pkgs.alacritty
                  pkgs.azure-cli
                  pkgs.bat
                  pkgs.bats
                  pkgs.brightnessctl
                  pkgs.buildah
                  pkgs.chart-testing
                  pkgs.chromium
                  pkgs.cilium-cli
                  pkgs.claude-code
                  pkgs.clipse
                  pkgs.cosign
                  pkgs.difftastic
                  pkgs.deadnix
                  pkgs.dig
                  pkgs.dive
                  pkgs.dunst
                  pkgs.dust
                  pkgs.dyff
                  pkgs.fd
                  pkgs.firefox
                  pkgs.fluxcd
                  pkgs.fzf
                  pkgs.kanshi
                  pkgs.keyd
                  pkgs.kitty
                  pkgs.gcolor3
                  pkgs.gammastep
                  pkgs.gimp
                  pkgs.gh
                  pkgs.gopass
                  pkgs.gomuks
                  pkgs.kubernetes-helm
                  pkgs.helm-docs
                  pkgs.imv
                  pkgs.jq
                  pkgs.jwt-cli
                  pkgs.kind
                  pkgs.kubectl
                  pkgs.kubectl-neat
                  pkgs.kubectx
                  pkgs.kubelogin
                  pkgs.kubelogin-oidc
                  pkgs.kustomize
                  pkgs.krew
                  pkgs.lazygit
                  pkgs.lsof
                  pkgs.lbdb
                  pkgs.libnotify
                  pkgs.libreoffice
                  pkgs.logseq
                  pkgs.nixd
                  pkgs.msmtp
                  #      pkgs.matterhorn
                  pkgs.notmuch
                  pkgs.notmuch-mutt
                  pkgs.oama
                  pkgs.openshift
                  pkgs.opencode
                  pkgs.openssl
                  pkgs.ponymix
                  pkgs.pipes-rs
                  pkgs.ripgrep
                  pkgs.offlineimap
                  pkgs.slack
                  #      pkgs.webdump
                  pkgs.ncpamixer
                  pkgs.neomutt
                  pkgs.neovim
                  pkgs.networkmanagerapplet
                  pkgs.niri
                  pkgs.nodejs_24
                  pkgs.pavucontrol
                  pkgs.podman
                  pkgs.podman-compose
                  pkgs.python3
                  pkgs.ruby
                  pkgs.openbao
                  pkgs.opentofu
                  pkgs.swaybg
                  pkgs.swaylock
                  pkgs.shell-gpt
                  pkgs.tmux
                  pkgs.unstable.tmuxinator
                  pkgs.wayneko
                  pkgs.wmenu
                  pkgs.windowtolayer
                  pkgs.yq-go
                  pkgs.zoxide
                  pkgs.zulip
                ];
                stateVersion = config.eyenx.stateVersion;
                username = config.eyenx.user.name;
                homeDirectory = config.eyenx.user.homeDirectory;
                sessionVariables = {
                  SOPS_AGE_KEY_FILE = config.eyenx.user.homeDirectory + "/.config/sops/age/keys.txt";
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
                  name = "Cozette";
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
              fonts.fontconfig.defaultFonts.monospace = [
                "Cozette"
                "Noto Color Emoji"
              ];
              fonts.fontconfig.defaultFonts.sansSerif = [
                "Cozette"
                "Noto Color Emoji"
              ];
              fonts.fontconfig.defaultFonts.serif = [
                "Cozette"
                "Noto Color Emoji"
              ];
              fonts.fontconfig.defaultFonts.emoji = [
                "Cozette"
                "Noto Color Emoji"
              ];
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
