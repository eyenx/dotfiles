return {
  { "towolf/vim-helm", lazy = false },
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        helm_ls = { yamlls = { path = "yaml-language-server" } },
      },
    },
  },
}
