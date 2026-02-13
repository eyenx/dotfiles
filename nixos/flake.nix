{
  description = "eyenx NixOS Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-main.url = "github:nixos/nixpkgs";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    impermanence = {
      url = "github:nix-community/impermanence";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hw = {
      url = "github:NixOS/nixos-hardware/master";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      nixpkgs-main,
      home-manager,
      impermanence,
      sops-nix,
      ...
    }@inputs:
    let
      inherit (self) outputs;
      forAllSystems = nixpkgs.lib.genAttrs [ "x86_64-linux" ];
      lib =
        system:
        nixpkgs.lib.recursiveUpdate (import ./lib {
          pkgs = nixpkgs.legacyPackages.${system};
          lib = nixpkgs.lib;
        }) nixpkgs.lib;

      # TODO
      privModules = [ ];
      modules = [
        (
          {
            inputs,
            outputs,
            lib,
            config,
            pkgs,
            ...
          }:
          {
            nixpkgs = {
              overlays = [
                (import ./overlays { inherit inputs; }).additions
                (import ./overlays { inherit inputs; }).unstable-packages
                (import ./overlays { inherit inputs; }).force-latest
              ];
            };
          }
        )
        sops-nix.nixosModules.sops
        impermanence.nixosModule
        home-manager.nixosModules.home-manager

        ./modules
        ./configuration.nix
      ];
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          rawModules = [ ./modules ];
        in
        (import ./pkgs { inherit pkgs; })
      );
      formatter = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        pkgs.nixfmt
      );

      # checks?

      # overlays
      overlays = import ./overlays { inherit inputs; };

      nixosConfigurations = {
        zelus = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
            lib = lib "x86_64-linux";
          };
          modules = privModules ++ modules ++ [ ./hosts/zelus/default.nix ];
        };
      };
    };
}
