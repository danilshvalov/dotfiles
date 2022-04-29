local map = vim.api.nvim_set_keymap
local opts = { silent = true, noremap = true }

map("n", "<Leader>lt", '<Cmd>lua require("lint").try_lint()<CR>', opts)

require("lint").linters_by_ft = { python = { "pylint" }, markdown = { "markdownlint" } }
