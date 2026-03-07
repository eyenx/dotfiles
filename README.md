# dotfiles

Personal dotfiles for [eyenx](https://github.com/eyenx), managed with a bare git repository and powered by a full [NixOS](https://nixos.org/) configuration.

## Overview

This repository contains configuration files and scripts for a complete, reproducible Linux desktop environment. The setup is centered around Wayland, Zsh, Neovim, and a range of DevOps/cloud tooling, tied together with a consistent Gruvbox dark theme and vi-style keybindings throughout.

## Highlights

- **Shell**: Zsh with [oh-my-zsh](https://ohmyz.sh/), vi-mode, 100+ aliases, and custom functions
- **Editor**: [Neovim](https://neovim.io/) with [AstroNvim v4](https://astronvim.com/) (Lua), Copilot, and community language packs
- **Multiplexer**: [Tmux](https://github.com/tmux/tmux) with vi keybindings and a custom status bar
- **Window Manager**: [Niri](https://github.com/YaLTeR/niri) (Wayland scrollable tiling WM)
- **System**: [NixOS](https://nixos.org/) flake with [home-manager](https://github.com/nix-community/home-manager), [SOPS](https://github.com/Mic92/sops-nix) secrets, and [impermanence](https://github.com/nix-community/impermanence)
- **Theme**: [Gruvbox](https://github.com/morhetz/gruvbox) across GTK, terminal, and browser

## Directory Structure

```
dotfiles/
├── .aliases             # 100+ shell aliases
├── .functs              # Custom Zsh/Bash functions
├── .gitconfig           # Git config (GPG signing, log aliases)
├── .mailcap             # MIME type associations
├── .myzshenv            # Zsh environment variables
├── .myzshrc             # Zsh configuration (oh-my-zsh, plugins)
├── .tmux.conf           # Tmux config (vi keys, custom status bar)
├── .tridactylrc         # Firefox vim keybindings (Tridactyl)
├── .config/
│   ├── dunst/           # Notification daemon (dunst)
│   ├── kanshi/          # Multi-monitor display profile manager
│   ├── niri/            # Niri window manager
│   ├── nvim/            # Neovim / AstroNvim
│   ├── tmuxinator/      # Preconfigured tmux sessions
│   └── waybar/          # Wayland status bar
├── .nixos/
│   ├── flake.nix        # Nix flake (nixpkgs 25.11, home-manager, sops-nix)
│   ├── configuration.nix
│   ├── hosts/zelus/     # Machine-specific hardware config
│   ├── modules/         # Modular NixOS/home-manager configs
│   ├── overlays/        # Custom package overlays
│   ├── pkgs/            # Custom packages
│   └── secrets/         # SOPS-encrypted secrets
├── .themes/gruvbox/     # GTK 2/3, GNOME Shell, Chrome Gruvbox theme
├── .icons/              # Icon themes
├── .gnupg/              # GPG agent configuration
└── bin/                 # Custom utility scripts
    ├── colorscheme      # Display 256-color palette
    ├── htmldmp          # HTML-to-text converter
    ├── mailfmt          # Mail text formatter
    ├── tmuxsess         # Smart tmux session launcher
    ├── wm               # Wayland app launcher (wmenu)
    ├── wmpass           # Password manager via wmenu
    └── wmtotp           # TOTP manager via wmenu
```

## Key Tools Configured

| Category | Tools |
|---|---|
| Shell | Zsh, oh-my-zsh, fzf, zoxide |
| Editor | Neovim (AstroNvim), vi-mode |
| Multiplexer | Tmux, Tmuxinator |
| Window Manager | Niri, Kanshi, Waybar, Dunst |
| Browser | Firefox + Tridactyl |
| Version Control | Git (GPG-signed commits) |
| DevOps | kubectl, kubectx, kubie, Helm, Terraform/OpenTofu, Ansible |
| Cloud | Azure CLI, OCI CLI, HashiCorp Vault |
| Containers | Podman, Nerdctl |
| Email | Neomutt (mu), Matterhorn, Gomuks |
| Passwords | pass / gopass, TOTP via wmtotp |
| Media | mplayer, ffmpeg, imv |
| AI | sgpt (ShellGPT), Copilot (Neovim) |
| System | NixOS, home-manager, SOPS secrets |

## Usage

Dotfiles are deployed using a **bare git repository** pattern. The `dit` alias wraps git for managing the dotfiles repo from `$HOME`:

```sh
alias dit='git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME'
```

### NixOS System

The `.nixos/` directory contains a complete NixOS flake. To rebuild the system:

```sh
cd ~/.nixos
nixos-rebuild switch --flake .#
```

### Shell

Source the shell files from your `.zshrc`:

```sh
source ~/.myzshenv
source ~/.myzshrc
```

### Tmux

Launch a smart tmux session (attaches to an existing one or creates a new one):

```sh
tmuxsess
```

Start the preconfigured mail session (neomutt + Matterhorn + Gomuks):

```sh
tmuxinator start mail
```

## Theming

The Gruvbox dark theme is applied consistently:

- **GTK 2/3**: via `.themes/gruvbox/`
- **Terminal/Neovim**: Gruvbox colorscheme
- **Browser**: Gruvbox Chrome theme

## License

These are personal configuration files shared as-is. Feel free to borrow anything useful.
