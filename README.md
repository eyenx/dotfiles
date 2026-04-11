# dotfiles

Personal dotfiles for [eyenx](https://github.com/eyenx), managed with a bare git repository and a full [NixOS](https://nixos.org/) flake configuration.

## Stack

- **Shell**: Zsh + oh-my-zsh, vi-mode, fzf, zoxide
- **Editor**: Neovim ([AstroNvim](https://astronvim.com/))
- **Multiplexer**: Tmux + Tmuxinator
- **Window Manager**: [Niri](https://github.com/YaLTeR/niri) (Wayland), Kanshi, Waybar, Dunst
- **Browser**: Firefox + [Tridactyl](https://github.com/tridactyl/tridactyl)
- **DevOps**: kubectl, kubectx, Helm, OpenTofu, Ansible
- **Cloud**: Azure CLI, OpenBao
- **Containers**: Podman
- **System**: NixOS flake, home-manager, SOPS secrets, impermanence
- **Theme**: Gruvbox

## Usage

Dotfiles are tracked with a bare git repository. The `dit` alias manages them from `$HOME`:

```sh
alias dit='git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME'
```

To rebuild the NixOS system:

```sh
nixos-rebuild switch --flake "/home/eye/.nixos#host" --impure --sudo
```
