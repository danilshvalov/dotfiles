-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

  local time
  local profile_info
  local should_profile = false
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/Users/danilshvalov/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/Users/danilshvalov/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/Users/danilshvalov/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/Users/danilshvalov/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/Users/danilshvalov/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s), name, _G.packer_plugins[name])
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  ["Comment.nvim"] = {
    config = { "\27LJ\2\n;\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0 vdk.plugins.configs.comment\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/Comment.nvim",
    url = "https://github.com/numToStr/Comment.nvim"
  },
  LuaSnip = {
    config = { "\27LJ\2\n;\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0 vdk.plugins.configs.luasnip\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/LuaSnip",
    url = "https://github.com/L3MON4D3/LuaSnip"
  },
  ["bufdelete.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/bufdelete.nvim",
    url = "https://github.com/famiu/bufdelete.nvim"
  },
  ["cmp-buffer"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/cmp-buffer",
    url = "https://github.com/hrsh7th/cmp-buffer"
  },
  ["cmp-cmdline"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/cmp-cmdline",
    url = "https://github.com/hrsh7th/cmp-cmdline"
  },
  ["cmp-nvim-lsp"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/cmp-nvim-lsp",
    url = "https://github.com/hrsh7th/cmp-nvim-lsp"
  },
  ["cmp-path"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/cmp-path",
    url = "https://github.com/hrsh7th/cmp-path"
  },
  cmp_luasnip = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/cmp_luasnip",
    url = "https://github.com/saadparwaiz1/cmp_luasnip"
  },
  ["diffview.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/diffview.nvim",
    url = "https://github.com/sindrets/diffview.nvim"
  },
  ["git-conflict.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/git-conflict.nvim",
    url = "https://github.com/akinsho/git-conflict.nvim"
  },
  ["github-nvim-theme"] = {
    config = { "\27LJ\2\n@\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0%vdk.plugins.configs.github-theme\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/github-nvim-theme",
    url = "https://github.com/projekt0n/github-nvim-theme"
  },
  ["gitsigns.nvim"] = {
    config = { "\27LJ\2\n<\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0!vdk.plugins.configs.gitsigns\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/gitsigns.nvim",
    url = "https://github.com/lewis6991/gitsigns.nvim"
  },
  ["impatient.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/impatient.nvim",
    url = "https://github.com/lewis6991/impatient.nvim"
  },
  ["indent-blankline.nvim"] = {
    config = { "\27LJ\2\nD\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0)vdk.plugins.configs.indent-blankline\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/indent-blankline.nvim",
    url = "https://github.com/lukas-reineke/indent-blankline.nvim"
  },
  ["leap.nvim"] = {
    config = { "\27LJ\2\n8\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\29vdk.plugins.configs.leap\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/leap.nvim",
    url = "https://github.com/ggandor/leap.nvim"
  },
  ["lsp-colors.nvim"] = {
    config = { "\27LJ\2\n>\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0#vdk.plugins.configs.lsp-colors\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/lsp-colors.nvim",
    url = "https://github.com/folke/lsp-colors.nvim"
  },
  ["lsp_signature.nvim"] = {
    config = { "\27LJ\2\nA\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0&vdk.plugins.configs.lsp-signature\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/lsp_signature.nvim",
    url = "https://github.com/ray-x/lsp_signature.nvim"
  },
  ["lspsaga.nvim"] = {
    config = { "\27LJ\2\n;\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0 vdk.plugins.configs.lspsaga\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/lspsaga.nvim",
    url = "https://github.com/tami5/lspsaga.nvim"
  },
  ["lualine.nvim"] = {
    config = { "\27LJ\2\n;\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0 vdk.plugins.configs.lualine\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/lualine.nvim",
    url = "https://github.com/diegodox/lualine.nvim"
  },
  ["move.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/move.nvim",
    url = "https://github.com/fedepujol/move.nvim"
  },
  ["neo-tree.nvim"] = {
    config = { "\27LJ\2\n<\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0!vdk.plugins.configs.neo-tree\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/neo-tree.nvim",
    url = "https://github.com/nvim-neo-tree/neo-tree.nvim"
  },
  neogen = {
    config = { "\27LJ\2\n:\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\31vdk.plugins.configs.neogen\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/neogen",
    url = "https://github.com/danymat/neogen"
  },
  neogit = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/neogit",
    url = "https://github.com/TimUntersberger/neogit"
  },
  neorg = {
    config = { "\27LJ\2\n9\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\30vdk.plugins.configs.neorg\frequire\0" },
    load_after = {},
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/neorg",
    url = "https://github.com/nvim-neorg/neorg"
  },
  ["nui.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/nui.nvim",
    url = "https://github.com/MunifTanjim/nui.nvim"
  },
  ["null-ls.nvim"] = {
    config = { "\27LJ\2\n;\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0 vdk.plugins.configs.null-ls\frequire\0" },
    load_after = {},
    loaded = true,
    needs_bufread = false,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/null-ls.nvim",
    url = "https://github.com/jose-elias-alvarez/null-ls.nvim"
  },
  ["nvim-autopairs"] = {
    config = { "\27LJ\2\nB\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0'vdk.plugins.configs.nvim-autopairs\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/nvim-autopairs",
    url = "https://github.com/windwp/nvim-autopairs"
  },
  ["nvim-cmp"] = {
    after = { "null-ls.nvim" },
    loaded = true,
    only_config = true
  },
  ["nvim-lsp-installer"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/nvim-lsp-installer",
    url = "https://github.com/williamboman/nvim-lsp-installer"
  },
  ["nvim-lspconfig"] = {
    config = { "\27LJ\2\n=\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\"vdk.plugins.configs.lspconfig\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/nvim-lspconfig",
    url = "https://github.com/neovim/nvim-lspconfig"
  },
  ["nvim-treesitter"] = {
    after = { "neorg" },
    loaded = true,
    only_config = true
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/nvim-web-devicons",
    url = "https://github.com/kyazdani42/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/packer.nvim",
    url = "https://github.com/wbthomason/packer.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/plenary.nvim",
    url = "https://github.com/nvim-lua/plenary.nvim"
  },
  ["popup.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/popup.nvim",
    url = "https://github.com/nvim-lua/popup.nvim"
  },
  ["reach.nvim"] = {
    config = { "\27LJ\2\n9\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\30vdk.plugins.configs.reach\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/reach.nvim",
    url = "https://github.com/toppair/reach.nvim"
  },
  ["sqlite.lua"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/sqlite.lua",
    url = "https://github.com/tami5/sqlite.lua"
  },
  ["telescope-fzf-native.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/telescope-fzf-native.nvim",
    url = "https://github.com/nvim-telescope/telescope-fzf-native.nvim"
  },
  ["telescope-project.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/telescope-project.nvim",
    url = "https://github.com/nvim-telescope/telescope-project.nvim"
  },
  ["telescope.nvim"] = {
    config = { "\27LJ\2\n=\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\"vdk.plugins.configs.telescope\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/telescope.nvim",
    url = "https://github.com/nvim-telescope/telescope.nvim"
  },
  ["tidy.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/tidy.nvim",
    url = "https://github.com/McAuleyPenney/tidy.nvim"
  },
  ["todo-comments.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/todo-comments.nvim",
    url = "https://github.com/folke/todo-comments.nvim"
  },
  ["vim-easy-align"] = {
    config = { "\27LJ\2\n9\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\30vdk.plugins.configs.align\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/vim-easy-align",
    url = "https://github.com/junegunn/vim-easy-align"
  },
  ["vim-nuuid"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/vim-nuuid",
    url = "https://github.com/kburdett/vim-nuuid"
  },
  ["vim-repeat"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/vim-repeat",
    url = "https://github.com/tpope/vim-repeat"
  },
  ["vim-startuptime"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/vim-startuptime",
    url = "https://github.com/dstein64/vim-startuptime"
  },
  vimtex = {
    config = { "\27LJ\2\n:\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\31vdk.plugins.configs.vimtex\frequire\0" },
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/vimtex",
    url = "https://github.com/lervag/vimtex"
  },
  ["virt-column.nvim"] = {
    config = { "\27LJ\2\n?\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0$vdk.plugins.configs.virt-column\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/virt-column.nvim",
    url = "https://github.com/lukas-reineke/virt-column.nvim"
  },
  ["which-key.nvim"] = {
    config = { "\27LJ\2\n7\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\1\2\0B\1\1\1K\0\1\0\nsetup\14which-key\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/which-key.nvim",
    url = "https://github.com/folke/which-key.nvim"
  },
  ["yanky.nvim"] = {
    config = { "\27LJ\2\n9\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\30vdk.plugins.configs.yanky\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/yanky.nvim",
    url = "https://github.com/gbprod/yanky.nvim"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: lsp-colors.nvim
time([[Config for lsp-colors.nvim]], true)
try_loadstring("\27LJ\2\n>\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0#vdk.plugins.configs.lsp-colors\frequire\0", "config", "lsp-colors.nvim")
time([[Config for lsp-colors.nvim]], false)
-- Config for: virt-column.nvim
time([[Config for virt-column.nvim]], true)
try_loadstring("\27LJ\2\n?\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0$vdk.plugins.configs.virt-column\frequire\0", "config", "virt-column.nvim")
time([[Config for virt-column.nvim]], false)
-- Config for: lsp_signature.nvim
time([[Config for lsp_signature.nvim]], true)
try_loadstring("\27LJ\2\nA\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0&vdk.plugins.configs.lsp-signature\frequire\0", "config", "lsp_signature.nvim")
time([[Config for lsp_signature.nvim]], false)
-- Config for: which-key.nvim
time([[Config for which-key.nvim]], true)
try_loadstring("\27LJ\2\n7\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\1\2\0B\1\1\1K\0\1\0\nsetup\14which-key\frequire\0", "config", "which-key.nvim")
time([[Config for which-key.nvim]], false)
-- Config for: lspsaga.nvim
time([[Config for lspsaga.nvim]], true)
try_loadstring("\27LJ\2\n;\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0 vdk.plugins.configs.lspsaga\frequire\0", "config", "lspsaga.nvim")
time([[Config for lspsaga.nvim]], false)
-- Config for: nvim-autopairs
time([[Config for nvim-autopairs]], true)
try_loadstring("\27LJ\2\nB\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0'vdk.plugins.configs.nvim-autopairs\frequire\0", "config", "nvim-autopairs")
time([[Config for nvim-autopairs]], false)
-- Config for: telescope.nvim
time([[Config for telescope.nvim]], true)
try_loadstring("\27LJ\2\n=\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\"vdk.plugins.configs.telescope\frequire\0", "config", "telescope.nvim")
time([[Config for telescope.nvim]], false)
-- Config for: lualine.nvim
time([[Config for lualine.nvim]], true)
try_loadstring("\27LJ\2\n;\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0 vdk.plugins.configs.lualine\frequire\0", "config", "lualine.nvim")
time([[Config for lualine.nvim]], false)
-- Config for: github-nvim-theme
time([[Config for github-nvim-theme]], true)
try_loadstring("\27LJ\2\n@\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0%vdk.plugins.configs.github-theme\frequire\0", "config", "github-nvim-theme")
time([[Config for github-nvim-theme]], false)
-- Config for: Comment.nvim
time([[Config for Comment.nvim]], true)
try_loadstring("\27LJ\2\n;\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0 vdk.plugins.configs.comment\frequire\0", "config", "Comment.nvim")
time([[Config for Comment.nvim]], false)
-- Config for: gitsigns.nvim
time([[Config for gitsigns.nvim]], true)
try_loadstring("\27LJ\2\n<\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0!vdk.plugins.configs.gitsigns\frequire\0", "config", "gitsigns.nvim")
time([[Config for gitsigns.nvim]], false)
-- Config for: neo-tree.nvim
time([[Config for neo-tree.nvim]], true)
try_loadstring("\27LJ\2\n<\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0!vdk.plugins.configs.neo-tree\frequire\0", "config", "neo-tree.nvim")
time([[Config for neo-tree.nvim]], false)
-- Config for: LuaSnip
time([[Config for LuaSnip]], true)
try_loadstring("\27LJ\2\n;\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0 vdk.plugins.configs.luasnip\frequire\0", "config", "LuaSnip")
time([[Config for LuaSnip]], false)
-- Config for: vim-easy-align
time([[Config for vim-easy-align]], true)
try_loadstring("\27LJ\2\n9\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\30vdk.plugins.configs.align\frequire\0", "config", "vim-easy-align")
time([[Config for vim-easy-align]], false)
-- Config for: nvim-cmp
time([[Config for nvim-cmp]], true)
try_loadstring("\27LJ\2\n<\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0!vdk.plugins.configs.nvim-cmp\frequire\0", "config", "nvim-cmp")
time([[Config for nvim-cmp]], false)
-- Config for: indent-blankline.nvim
time([[Config for indent-blankline.nvim]], true)
try_loadstring("\27LJ\2\nD\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0)vdk.plugins.configs.indent-blankline\frequire\0", "config", "indent-blankline.nvim")
time([[Config for indent-blankline.nvim]], false)
-- Config for: neogen
time([[Config for neogen]], true)
try_loadstring("\27LJ\2\n:\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\31vdk.plugins.configs.neogen\frequire\0", "config", "neogen")
time([[Config for neogen]], false)
-- Config for: nvim-treesitter
time([[Config for nvim-treesitter]], true)
try_loadstring("\27LJ\2\nC\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0(vdk.plugins.configs.nvim-treesitter\frequire\0", "config", "nvim-treesitter")
time([[Config for nvim-treesitter]], false)
-- Config for: leap.nvim
time([[Config for leap.nvim]], true)
try_loadstring("\27LJ\2\n8\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\29vdk.plugins.configs.leap\frequire\0", "config", "leap.nvim")
time([[Config for leap.nvim]], false)
-- Config for: yanky.nvim
time([[Config for yanky.nvim]], true)
try_loadstring("\27LJ\2\n9\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\30vdk.plugins.configs.yanky\frequire\0", "config", "yanky.nvim")
time([[Config for yanky.nvim]], false)
-- Config for: reach.nvim
time([[Config for reach.nvim]], true)
try_loadstring("\27LJ\2\n9\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\30vdk.plugins.configs.reach\frequire\0", "config", "reach.nvim")
time([[Config for reach.nvim]], false)
-- Config for: nvim-lspconfig
time([[Config for nvim-lspconfig]], true)
try_loadstring("\27LJ\2\n=\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\"vdk.plugins.configs.lspconfig\frequire\0", "config", "nvim-lspconfig")
time([[Config for nvim-lspconfig]], false)
-- Load plugins in order defined by `after`
time([[Sequenced loading]], true)
vim.cmd [[ packadd null-ls.nvim ]]

-- Config for: null-ls.nvim
try_loadstring("\27LJ\2\n;\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0 vdk.plugins.configs.null-ls\frequire\0", "config", "null-ls.nvim")

time([[Sequenced loading]], false)
vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
time([[Defining lazy-load filetype autocommands]], true)
vim.cmd [[au FileType norg ++once lua require("packer.load")({'neorg'}, { ft = "norg" }, _G.packer_plugins)]]
vim.cmd [[au FileType tex ++once lua require("packer.load")({'vimtex'}, { ft = "tex" }, _G.packer_plugins)]]
time([[Defining lazy-load filetype autocommands]], false)
vim.cmd("augroup END")
vim.cmd [[augroup filetypedetect]]
time([[Sourcing ftdetect script at: /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/neorg/ftdetect/norg.vim]], true)
vim.cmd [[source /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/neorg/ftdetect/norg.vim]]
time([[Sourcing ftdetect script at: /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/neorg/ftdetect/norg.vim]], false)
time([[Sourcing ftdetect script at: /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/vimtex/ftdetect/cls.vim]], true)
vim.cmd [[source /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/vimtex/ftdetect/cls.vim]]
time([[Sourcing ftdetect script at: /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/vimtex/ftdetect/cls.vim]], false)
time([[Sourcing ftdetect script at: /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/vimtex/ftdetect/tex.vim]], true)
vim.cmd [[source /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/vimtex/ftdetect/tex.vim]]
time([[Sourcing ftdetect script at: /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/vimtex/ftdetect/tex.vim]], false)
time([[Sourcing ftdetect script at: /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/vimtex/ftdetect/tikz.vim]], true)
vim.cmd [[source /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/vimtex/ftdetect/tikz.vim]]
time([[Sourcing ftdetect script at: /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/vimtex/ftdetect/tikz.vim]], false)
vim.cmd("augroup END")
if should_profile then save_profiles() end

end)

if not no_errors then
  error_msg = error_msg:gsub('"', '\\"')
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
