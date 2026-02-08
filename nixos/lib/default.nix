{ lib, pkgs, ... }:
{
  eyenx = {
    isLinux = pkgs.stdenv.isLinux;
  };
}
