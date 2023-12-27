return {
  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    opts = function()
      return {
        options = {
          theme = "auto",
          section_separators = "",
          component_separators = "",
        },
      }
    end,
  },
}
