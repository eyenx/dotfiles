-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
vim.api.nvim_set_keymap("n", "<C-b>", ":AI ", { noremap = true })
vim.api.nvim_set_keymap("v", "<C-b>", ":AI ", { noremap = true })
vim.api.nvim_set_keymap("i", "<C-b>", "<Esc>:AI<CR>a", { noremap = true })
