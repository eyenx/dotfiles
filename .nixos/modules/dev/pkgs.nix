{
  config,
  lib,
  pkgs,
  ...
}:
{
  options = {
    eyenx.devpkgs = {
      enable = lib.mkEnableOption "shared development packages";
      tools = {
        nix = lib.mkEnableOption "Nix development tools";
        c = lib.mkEnableOption "C development";
        go = lib.mkEnableOption "Go development";
        python = lib.mkEnableOption "Python development";
        rust = lib.mkEnableOption "Rust development";
        iac = lib.mkEnableOption "IaC tools";
        k8s = lib.mkEnableOption "Kubernetes tools";
        cloud = lib.mkEnableOption "Cloud tools";
        dev = lib.mkEnableOption "Dev Tools";
        security = lib.mkEnableOption "Security tools";
        networking = lib.mkEnableOption "Networking tools";
        database = lib.mkEnableOption "Database tools";
      };
    };
  };

  config = lib.mkIf config.eyenx.devpkgs.enable {
    home-manager.users.${config.eyenx.user.name} = {
      home.packages =
        with pkgs;
        lib.flatten [
          # Nix Tools
          (lib.optionals config.eyenx.devpkgs.tools.nix [
            nixfmt
          ])
          # C Tools
          (lib.optionals config.eyenx.devpkgs.tools.c [
            gcc
            gnumake
          ])
          # Go Tools
          (lib.optionals config.eyenx.devpkgs.tools.go [
            go
            golangci-lint
          ])
          # Python Tools
          (lib.optionals config.eyenx.devpkgs.tools.python [
            python3
            python3Packages.python-lsp-server
            python3Packages.python-lsp-ruff
            python3Packages.pwntools
            uv
          ])
          # IaC Tools
          (lib.optionals config.eyenx.devpkgs.tools.iac [
            opentofu
          ])
          # K8s Tools
          (lib.optionals config.eyenx.devpkgs.tools.k8s [
            dive
            helm-docs
            kind
            kubectl
            kubectl-neat
            kubectl-view-secret
            kubectx
            kubelogin
            kubelogin-oidc
            kubernetes-helm
            kustomize
            krew
            stern
          ])
          # Cloud CLI Tools
          (lib.optionals config.eyenx.devpkgs.tools.cloud [
            awscli2
            azure-cli
          ])
          # Dev Tools
          (lib.optionals config.eyenx.devpkgs.tools.dev [
            bats
            pre-commit
            yq-go
          ])
          # Security Tools
          (lib.optionals config.eyenx.devpkgs.tools.security [
            openbao
          ])
          # Database Tools
          (lib.optionals config.eyenx.devpkgs.tools.database [ postgresql ])
        ];
    };
  };
}
