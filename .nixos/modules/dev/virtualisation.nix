{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    eyenx.virtualisation = {
      hypervisor.enable = lib.mkEnableOption "Libvirt/KVM";
    };
  };

  config = lib.mkIf config.eyenx.virtualisation.hypervisor.enable {

    virtualisation.libvirtd = {
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        swtpm.enable = true;
        runAsRoot = false;
      };

      onBoot = "start";
      onShutdown = "shutdown";
    };

    home-manager.users.${config.eyenx.user.name} = {
      home.packages = with pkgs; [ virt-manager ];
    };

    #    users.users.eyenx = {
    #  extraGroups = [ "libvirtd" ];
    #};
  };
}
