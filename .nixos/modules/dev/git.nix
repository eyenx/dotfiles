{
  config,
  ...
}:
{
  config = {
    home-manager.users.${config.eyenx.user.name} = {
      programs = {
        git = {
          enable = true;
          lfs.enable = true;

          settings = {
            init.defaultBranch = "main";
            push.autoSetupRemote = true;
            pull.rebase = true;

            #user.signingKey = "~/.ssh/id_ed25519.pub";
            user.email = config.eyenx.user.email;
            user.name = config.eyenx.user.fullName;

            #safe.directory = "${config.eyenx.user.homeDirectory}/${config.eyenx.user.codeDir}/nixos-config";

            #gpg.format = "ssh";
            commit.gpgsign = true;
          };
        };

        lazygit = {
          enable = true;
          settings = {
            git = {
              commit = {
                signOff = true;
              };
            };
          };
        };

        gh = {
          enable = true;
          settings = {
            editor = "nvim";
            git_protocol = "ssh";
          };
        };
      };
    };
  };
}
