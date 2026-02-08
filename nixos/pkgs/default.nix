{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    acpi
    gcc
    file
    epson-escpr2
    git
    gnumake
    gtk3
    gtk4
    gtk-engine-murrine
    htop
    vim
    unzip
    wget
    wireguard-tools
    ydotool
    xwayland-satellite
    wl-clipboard
    zip
  ];
}
