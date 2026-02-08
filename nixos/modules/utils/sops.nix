{ config, lib, ... }: {
  sops = {
    defaultSopsFile = ../../secrets/secrets.yaml;
    defaultSopsFormat = "yaml";
    secrets = lib.mkMerge [ 
      {"users/root" = { neededForUsers = true; };}
      {"users/eye" = { neededForUsers = true; };}
    ];
  };
}
