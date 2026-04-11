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
            user.email = config.eyenx.user.email;
            user.name = config.eyenx.user.fullName;
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
