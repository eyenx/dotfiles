---@type LazySpec
return {
  "AstroNvim/astrocommunity",
  { import = "astrocommunity.pack.lua" },
  { import = "astrocommunity.pack.terraform" },
  { import = "astrocommunity.pack.helm" },
  { import = "astrocommunity.pack.python" },
  { import = "astrocommunity.pack.bash" },
  { import = "astrocommunity.pack.yaml" },
  { import = "astrocommunity.completion.copilot-lua-cmp" },
  { import = "astrocommunity.editing-support.chatgpt-nvim" },
  { import = "astrocommunity.note-taking.obsidian-nvim" },
}
