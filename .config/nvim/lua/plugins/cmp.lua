return { -- override nvim-cmp plugin
  "hrsh7th/nvim-cmp",
  -- override the options table that is used in the `require("cmp").setup()` call
  opts = function(_, opts)
    --    opts.sources = {
    --      {
    --        { name = "minuet" },
    --      },
    --    }
    --    opts.performance = {
    --      fetching_timeout = 2000,
    --    }
    local cmp = require "cmp"
    -- modify the mapping part of the table
    opts.mapping = vim.tbl_deep_extend("force", opts.mapping or {}, {
      ["<C-y>"] = cmp.mapping.confirm { select = true },
      --      ["<A-y>"] = require("minuet").make_cmp_map(),
    })
  end,
}
