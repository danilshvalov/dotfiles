local opts = { silent = true, noremap = true }
local map = vim.api.nvim_set_keymap

-- map("n", "<C-n>", "<Cmd>NvimTreeToggle<CR>", opts)
-- map("n", "<C-A-n>", "<Cmd>NvimTreeFindFileToggle<CR>", opts)
-- map("n", "<leader>r", "<Cmd>NvimTreeRefresh<CR>", opts)

-- map('n', 'm', '<Cmd>lua require("nvim-tree.marks").toggle_mark()', opts)

-- find the currently open file in tree
-- map('n', '<leader>n', '<Cmd>NvimTreeFindFile<CR>', opts)
-- local tree_cb = require'nvim-tree.config'.nvim_tree_callback
-- vim.g.nvim_tree_bindings = {
--  { key = "<C-t>", cb = tree_cb("tabnew") },
--  { key = "<CR>", cb = tree_cb("edit") },
--  { key = "o", cb = tree_cb("edit") },
--  { key = "<2-LeftMouse>", cb = tree_cb("edit") },
--  { key = "<2-RightMouse>", cb = tree_cb("cd") },
--  { key = "<Tab>", cb = tree_cb("preview") },
--  { key = "R", cb = tree_cb("refresh") },
--  { key = "a", cb = tree_cb("create") },
--  { key = "d", cb = tree_cb("remove") },
--  { key = "r", cb = tree_cb("rename") },
--  { key = "x", cb = tree_cb("cut") },
--  { key = "y", cb = tree_cb("copy") },
--  { key = "p", cb = tree_cb("paste") },
--  { key = "<", cb = tree_cb("dir_up") },
--  { key = "q", cb = tree_cb("close") }
-- }
-- vim.g.nvim_tree_auto_close = 1 -- close tree when it's the last window

--[[ require("nvim-tree.events").on_nvim_tree_ready(function()
    vim.cmd("NvimTreeRefresh")
end) ]]

vim.g.nvim_tree_indent_markers = 1
vim.g.nvim_tree_quit_on_open = 1
vim.g.nvim_tree_git_hl = 1

vim.g.nvim_tree_show_icons = { folders = 1, folder_arrows = 1, files = 1 }

vim.g.nvim_tree_icons = {
  default = "",
  symlink = "",
  git = {
    deleted = "",
    ignored = "◌",
    renamed = "➜",
    staged = "✓",
    unmerged = "",
    unstaged = "✗",
    untracked = "★",
  },
  folder = {
    -- disable indent_markers option to get arrows working or if you want both arrows and indent then just add the arrow icons in front            ofthe default and opened folders below!
    --[[ arrow_open = "",
        arrow_closed = "", ]]
    default = "",
    empty = "", -- 
    empty_open = "",
    open = "",
    symlink = "",
    symlink_open = "",
  },
}
-- following options are the default
require("nvim-tree").setup({
  -- disables netrw completely
  disable_netrw = true,
  -- hijack netrw window on startup
  hijack_netrw = true,
  -- open the tree when running this setup function
  open_on_setup = false,
  -- will not open on setup if the filetype is in this list
  ignore_ft_on_setup = {},
  -- closes neovim automatically when the tree is the last **WINDOW** in the view
  auto_close = false,
  -- opens the tree when changing/opening a new tab if the tree wasn't previously opened
  open_on_tab = false,
  -- hijacks new directory buffers when they are opened.
  update_to_buf_dir = {
    -- enable the feature
    enable = true,
    -- allow to open the tree if it was previously closed
    auto_open = true,
  },
  -- hijack the cursor in the tree to put it at the start of the filename
  hijack_cursor = false,
  -- updates the root directory of the tree on `DirChanged` (when your run `:cd` usually)
  update_cwd = true,
  -- show lsp diagnostics in the signcolumn
  diagnostics = {
    enable = true,
    icons = { hint = "", info = "", warning = "", error = "" },
  },
  -- update the focused file on `BufEnter`, un-collapses the folders recursively until it finds the file
  update_focused_file = {
    -- enables the feature
    enable = false,
    -- update the root directory of the tree to the one of the folder containing the file if the file is not under the current root directory
    -- only relevant when `update_focused_file.enable` is true
    update_cwd = false,
    -- list of buffer names / filetypes that will not update the cwd if the file isn't found under the current root directory
    -- only relevant when `update_focused_file.update_cwd` is true and `update_focused_file.enable` is true
    ignore_list = {},
  },
  -- configuration options for the system open command (`s` in the tree by default)
  system_open = {
    -- the command to run this, leaving nil should work in most cases
    cmd = nil,
    -- the command arguments as a list
    args = {},
  },

  view = {
    -- width of the window, can be either a number (columns) or a string in `%`, for left or right side placement
    width = 30,
    -- height of the window, can be either a number (columns) or a string in `%`, for top or bottom side placement
    height = 30,
    -- Hide the root path of the current folder on top of the tree
    hide_root_folder = false,
    -- side of the tree, can be one of 'left' | 'right' | 'top' | 'bottom'
    side = "right",
    -- if true the tree will resize itself after opening a file
    auto_resize = false,
    mappings = {
      -- custom only false will merge the list with the default mappings
      -- if true, it will only use your list to set the mappings
      custom_only = false,
      -- list of mappings to set on the tree manually
      list = {},
    },
  },

  filters = {
    dotfiles = false,
    custom = { "*.class" },
  },
  git = {
    enable = true,
    ignore = false,
    timeout = 500,
  },
})
