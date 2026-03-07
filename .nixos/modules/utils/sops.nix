{ config, lib, ... }:
{
  sops = {
    defaultSopsFile = ../../secrets/secrets.yaml;
    defaultSopsFormat = "yaml";
    age = {
      keyFile = "/persist/age/keys.txt";
      generateKey = true;
    };
    secrets = lib.mkMerge [
      {
        "users/root" = {
          neededForUsers = true;
        };
      }
      {
        "users/eye" = {
          neededForUsers = true;
        };
      }
    ];
  };
}
