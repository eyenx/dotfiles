return {
  "neorg",
  dependencies = { "nvim-treesitter/nvim-treesitter", "vhyrro/luarocks.nvim" },
  opts = {
    load = {
      ["core.journal"] = {
        config = {
          workspace = "notes",
          strategy = "flat",
        },
      }, -- Enables support for the journal module
      ["core.dirman"] = { -- Manages Neorg workspaces
        config = {
          default_workspace = "notes",
          workspaces = {
            notes = "~/.neorg",
          },
        },
      },
    },
  },
}
