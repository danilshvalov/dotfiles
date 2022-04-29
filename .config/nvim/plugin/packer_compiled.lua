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
  ["bufdelete.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/bufdelete.nvim",
    url = "https://github.com/famiu/bufdelete.nvim"
  },
  ["bufferline.nvim"] = {
    config = { "\27LJ\2\nC\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\30floppy.configs.bufferline\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/bufferline.nvim",
    url = "https://github.com/akinsho/bufferline.nvim"
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
  ["cmp-snippy"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/cmp-snippy",
    url = "https://github.com/dcampos/cmp-snippy"
  },
  ["conflict-marker.vim"] = {
    config = { "\27LJ\2\nH\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup#floppy.configs.conflict-marker\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/conflict-marker.vim",
    url = "https://github.com/rhysd/conflict-marker.vim"
  },
  ["diffview.nvim"] = {
    config = { "\27LJ\2\nA\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\28floppy.configs.diffview\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/diffview.nvim",
    url = "https://github.com/sindrets/diffview.nvim"
  },
  ["fidget.nvim"] = {
    config = { "\27LJ\2\n0\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\vfidget\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/fidget.nvim",
    url = "https://github.com/j-hui/fidget.nvim"
  },
  ["gitsigns.nvim"] = {
    config = { "\27LJ\2\n2\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\rgitsigns\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/gitsigns.nvim",
    url = "https://github.com/lewis6991/gitsigns.nvim"
  },
  ["hibiscus.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/hibiscus.nvim",
    url = "https://github.com/udayvir-singh/hibiscus.nvim"
  },
  ["indent-blankline.nvim"] = {
    config = { "\27LJ\2\nI\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup$floppy.configs.indent-blankline\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/indent-blankline.nvim",
    url = "https://github.com/lukas-reineke/indent-blankline.nvim"
  },
  kommentary = {
    config = { "\27LJ\2\nx\0\0\5\0\5\0\a6\0\0\0'\2\1\0B\0\2\0029\1\2\0'\3\3\0005\4\4\0D\1\3\0\1\0\1 prefer_single_line_comments\2\fdefault\23configure_language\22kommentary.config\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/kommentary",
    url = "https://github.com/b3nj5m1n/kommentary"
  },
  ["leap.nvim"] = {
    config = { "\27LJ\2\n=\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\24floppy.configs.leap\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/leap.nvim",
    url = "https://github.com/ggandor/leap.nvim"
  },
  ["lsp-colors.nvim"] = {
    config = { "\27LJ\2\nC\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\30floppy.configs.lsp-colors\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/lsp-colors.nvim",
    url = "https://github.com/folke/lsp-colors.nvim"
  },
  ["lsp-trouble.nvim"] = {
    config = { "\27LJ\2\nD\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\31floppy.configs.lsp-trouble\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/lsp-trouble.nvim",
    url = "https://github.com/folke/lsp-trouble.nvim"
  },
  ["lsp_signature.nvim"] = {
    config = { "\27LJ\2\n7\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\18lsp_signature\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/lsp_signature.nvim",
    url = "https://github.com/ray-x/lsp_signature.nvim"
  },
  ["lspsaga.nvim"] = {
    config = { "\27LJ\2\n@\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\27floppy.configs.lspsaga\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/lspsaga.nvim",
    url = "https://github.com/tami5/lspsaga.nvim"
  },
  ["lua-dev.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/lua-dev.nvim",
    url = "https://github.com/folke/lua-dev.nvim"
  },
  ["lualine.nvim"] = {
    config = { "\27LJ\2\n@\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\27floppy.configs.lualine\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/lualine.nvim",
    url = "https://github.com/nvim-lualine/lualine.nvim"
  },
  ["move.nvim"] = {
    config = { "\27LJ\2\n=\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\24floppy.configs.move\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/move.nvim",
    url = "https://github.com/fedepujol/move.nvim"
  },
  ["neo-tree.nvim"] = {
    config = { "\27LJ\2\nA\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\28floppy.configs.neo-tree\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/neo-tree.nvim",
    url = "https://github.com/nvim-neo-tree/neo-tree.nvim"
  },
  neogit = {
    config = { "\27LJ\2\n4\0\0\4\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\1\2\0004\3\0\0D\1\2\0\nsetup\vneogit\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/neogit",
    url = "https://github.com/TimUntersberger/neogit"
  },
  ["nui.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/nui.nvim",
    url = "https://github.com/MunifTanjim/nui.nvim"
  },
  ["null-ls.nvim"] = {
    config = { "\27LJ\2\n@\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\27floppy.configs.null-ls\frequire\0" },
    load_after = {},
    loaded = true,
    needs_bufread = false,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/null-ls.nvim",
    url = "https://github.com/jose-elias-alvarez/null-ls.nvim"
  },
  ["nvim-autopairs"] = {
    config = { "\27LJ\2\nG\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\"floppy.configs.nvim-autopairs\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/nvim-autopairs",
    url = "https://github.com/windwp/nvim-autopairs"
  },
  ["nvim-cmp"] = {
    after = { "null-ls.nvim" },
    loaded = true,
    only_config = true
  },
  ["nvim-config-local"] = {
    config = { "\27LJ\2\nÙ\1\0\0\a\0\f\0\0176\0\0\0'\2\1\0B\0\2\0029\1\2\0005\3\4\0005\4\3\0=\4\5\0036\4\6\0009\4\a\0049\4\b\4'\6\t\0B\4\2\2'\5\n\0'\6\1\0&\4\6\4=\4\v\3D\1\2\0\rhashfile\6/\tdata\fstdpath\afn\bvim\17config_files\1\0\3\vsilent\1\20commands_create\2\24autocommands_create\2\1\3\0\0\15.vimrc.lua\v.vimrc\nsetup\17config-local\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/nvim-config-local",
    url = "https://github.com/klen/nvim-config-local"
  },
  ["nvim-lsp-installer"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/nvim-lsp-installer",
    url = "https://github.com/williamboman/nvim-lsp-installer"
  },
  ["nvim-lsp-ts-utils"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/nvim-lsp-ts-utils",
    url = "https://github.com/jose-elias-alvarez/nvim-lsp-ts-utils"
  },
  ["nvim-lspconfig"] = {
    after = { "rust-tools.nvim" },
    loaded = true,
    only_config = true
  },
  ["nvim-snippy"] = {
    config = { "\27LJ\2\n?\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\26floppy.configs.snippy\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/nvim-snippy",
    url = "https://github.com/dcampos/nvim-snippy"
  },
  ["nvim-treesitter"] = {
    config = { "\27LJ\2\nH\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup#floppy.configs.nvim-treesitter\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/nvim-treesitter",
    url = "https://github.com/nvim-treesitter/nvim-treesitter"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/nvim-web-devicons",
    url = "https://github.com/kyazdani42/nvim-web-devicons"
  },
  orgmode = {
    config = { "\27LJ\2\nN\0\0\4\0\4\0\b6\0\0\0'\2\1\0B\0\2\0029\1\2\0B\1\1\0019\1\3\0004\3\0\0D\1\2\0\nsetup\21setup_ts_grammar\forgmode\frequire\0" },
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/orgmode",
    url = "https://github.com/nvim-orgmode/orgmode"
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
  ["rust-tools.nvim"] = {
    config = { "\27LJ\2\nC\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\30floppy.configs.rust-tools\frequire\0" },
    load_after = {},
    loaded = true,
    needs_bufread = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/rust-tools.nvim",
    url = "https://github.com/simrat39/rust-tools.nvim"
  },
  ["sqlite.lua"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/sqlite.lua",
    url = "https://github.com/tami5/sqlite.lua"
  },
  ["tangerine.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/tangerine.nvim",
    url = "https://github.com/udayvir-singh/tangerine.nvim"
  },
  ["telescope-project.nvim"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/telescope-project.nvim",
    url = "https://github.com/nvim-telescope/telescope-project.nvim"
  },
  ["telescope.nvim"] = {
    config = { "\27LJ\2\nB\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\29floppy.configs.telescope\frequire\0" },
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
    config = { "\27LJ\2\nF\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup!floppy.configs.todo-comments\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/todo-comments.nvim",
    url = "https://github.com/folke/todo-comments.nvim"
  },
  ["tokyonight.nvim"] = {
    config = { "\27LJ\2\nO\0\0\3\0\5\0\a6\0\0\0009\0\1\0009\0\2\0006\0\0\0009\0\3\0'\2\4\0D\0\2\0\27colorscheme tokyonight\bcmd\15background\6o\bvim\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/tokyonight.nvim",
    url = "https://github.com/folke/tokyonight.nvim"
  },
  ["vim-repeat"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/vim-repeat",
    url = "https://github.com/tpope/vim-repeat"
  },
  ["vim-surround"] = {
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/vim-surround",
    url = "https://github.com/tpope/vim-surround"
  },
  vimtex = {
    config = { "\27LJ\2\n?\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\26floppy.configs.vimtex\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/vimtex",
    url = "https://github.com/lervag/vimtex"
  },
  ["virt-column.nvim"] = {
    config = { "\27LJ\2\nD\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\31floppy.configs.virt-column\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/virt-column.nvim",
    url = "https://github.com/lukas-reineke/virt-column.nvim"
  },
  ["which-key.nvim"] = {
    config = { "\27LJ\2\nB\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\29floppy.configs.which-key\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/which-key.nvim",
    url = "https://github.com/folke/which-key.nvim"
  },
  ["yanky.nvim"] = {
    config = { "\27LJ\2\n>\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\25floppy.configs.yanky\frequire\0" },
    loaded = true,
    path = "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/yanky.nvim",
    url = "https://github.com/gbprod/yanky.nvim"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: indent-blankline.nvim
time([[Config for indent-blankline.nvim]], true)
try_loadstring("\27LJ\2\nI\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup$floppy.configs.indent-blankline\frequire\0", "config", "indent-blankline.nvim")
time([[Config for indent-blankline.nvim]], false)
-- Config for: todo-comments.nvim
time([[Config for todo-comments.nvim]], true)
try_loadstring("\27LJ\2\nF\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup!floppy.configs.todo-comments\frequire\0", "config", "todo-comments.nvim")
time([[Config for todo-comments.nvim]], false)
-- Config for: lualine.nvim
time([[Config for lualine.nvim]], true)
try_loadstring("\27LJ\2\n@\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\27floppy.configs.lualine\frequire\0", "config", "lualine.nvim")
time([[Config for lualine.nvim]], false)
-- Config for: neogit
time([[Config for neogit]], true)
try_loadstring("\27LJ\2\n4\0\0\4\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\1\2\0004\3\0\0D\1\2\0\nsetup\vneogit\frequire\0", "config", "neogit")
time([[Config for neogit]], false)
-- Config for: tokyonight.nvim
time([[Config for tokyonight.nvim]], true)
try_loadstring("\27LJ\2\nO\0\0\3\0\5\0\a6\0\0\0009\0\1\0009\0\2\0006\0\0\0009\0\3\0'\2\4\0D\0\2\0\27colorscheme tokyonight\bcmd\15background\6o\bvim\0", "config", "tokyonight.nvim")
time([[Config for tokyonight.nvim]], false)
-- Config for: move.nvim
time([[Config for move.nvim]], true)
try_loadstring("\27LJ\2\n=\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\24floppy.configs.move\frequire\0", "config", "move.nvim")
time([[Config for move.nvim]], false)
-- Config for: leap.nvim
time([[Config for leap.nvim]], true)
try_loadstring("\27LJ\2\n=\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\24floppy.configs.leap\frequire\0", "config", "leap.nvim")
time([[Config for leap.nvim]], false)
-- Config for: kommentary
time([[Config for kommentary]], true)
try_loadstring("\27LJ\2\nx\0\0\5\0\5\0\a6\0\0\0'\2\1\0B\0\2\0029\1\2\0'\3\3\0005\4\4\0D\1\3\0\1\0\1 prefer_single_line_comments\2\fdefault\23configure_language\22kommentary.config\frequire\0", "config", "kommentary")
time([[Config for kommentary]], false)
-- Config for: nvim-snippy
time([[Config for nvim-snippy]], true)
try_loadstring("\27LJ\2\n?\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\26floppy.configs.snippy\frequire\0", "config", "nvim-snippy")
time([[Config for nvim-snippy]], false)
-- Config for: nvim-cmp
time([[Config for nvim-cmp]], true)
try_loadstring("\27LJ\2\nA\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\28floppy.configs.nvim-cmp\frequire\0", "config", "nvim-cmp")
time([[Config for nvim-cmp]], false)
-- Config for: conflict-marker.vim
time([[Config for conflict-marker.vim]], true)
try_loadstring("\27LJ\2\nH\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup#floppy.configs.conflict-marker\frequire\0", "config", "conflict-marker.vim")
time([[Config for conflict-marker.vim]], false)
-- Config for: nvim-lspconfig
time([[Config for nvim-lspconfig]], true)
try_loadstring("\27LJ\2\nB\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\29floppy.configs.lspconfig\frequire\0", "config", "nvim-lspconfig")
time([[Config for nvim-lspconfig]], false)
-- Config for: nvim-treesitter
time([[Config for nvim-treesitter]], true)
try_loadstring("\27LJ\2\nH\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup#floppy.configs.nvim-treesitter\frequire\0", "config", "nvim-treesitter")
time([[Config for nvim-treesitter]], false)
-- Config for: lsp-trouble.nvim
time([[Config for lsp-trouble.nvim]], true)
try_loadstring("\27LJ\2\nD\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\31floppy.configs.lsp-trouble\frequire\0", "config", "lsp-trouble.nvim")
time([[Config for lsp-trouble.nvim]], false)
-- Config for: diffview.nvim
time([[Config for diffview.nvim]], true)
try_loadstring("\27LJ\2\nA\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\28floppy.configs.diffview\frequire\0", "config", "diffview.nvim")
time([[Config for diffview.nvim]], false)
-- Config for: lsp-colors.nvim
time([[Config for lsp-colors.nvim]], true)
try_loadstring("\27LJ\2\nC\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\30floppy.configs.lsp-colors\frequire\0", "config", "lsp-colors.nvim")
time([[Config for lsp-colors.nvim]], false)
-- Config for: virt-column.nvim
time([[Config for virt-column.nvim]], true)
try_loadstring("\27LJ\2\nD\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\31floppy.configs.virt-column\frequire\0", "config", "virt-column.nvim")
time([[Config for virt-column.nvim]], false)
-- Config for: lsp_signature.nvim
time([[Config for lsp_signature.nvim]], true)
try_loadstring("\27LJ\2\n7\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\18lsp_signature\frequire\0", "config", "lsp_signature.nvim")
time([[Config for lsp_signature.nvim]], false)
-- Config for: fidget.nvim
time([[Config for fidget.nvim]], true)
try_loadstring("\27LJ\2\n0\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\vfidget\frequire\0", "config", "fidget.nvim")
time([[Config for fidget.nvim]], false)
-- Config for: telescope.nvim
time([[Config for telescope.nvim]], true)
try_loadstring("\27LJ\2\nB\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\29floppy.configs.telescope\frequire\0", "config", "telescope.nvim")
time([[Config for telescope.nvim]], false)
-- Config for: which-key.nvim
time([[Config for which-key.nvim]], true)
try_loadstring("\27LJ\2\nB\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\29floppy.configs.which-key\frequire\0", "config", "which-key.nvim")
time([[Config for which-key.nvim]], false)
-- Config for: lspsaga.nvim
time([[Config for lspsaga.nvim]], true)
try_loadstring("\27LJ\2\n@\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\27floppy.configs.lspsaga\frequire\0", "config", "lspsaga.nvim")
time([[Config for lspsaga.nvim]], false)
-- Config for: gitsigns.nvim
time([[Config for gitsigns.nvim]], true)
try_loadstring("\27LJ\2\n2\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\rgitsigns\frequire\0", "config", "gitsigns.nvim")
time([[Config for gitsigns.nvim]], false)
-- Config for: yanky.nvim
time([[Config for yanky.nvim]], true)
try_loadstring("\27LJ\2\n>\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\25floppy.configs.yanky\frequire\0", "config", "yanky.nvim")
time([[Config for yanky.nvim]], false)
-- Config for: neo-tree.nvim
time([[Config for neo-tree.nvim]], true)
try_loadstring("\27LJ\2\nA\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\28floppy.configs.neo-tree\frequire\0", "config", "neo-tree.nvim")
time([[Config for neo-tree.nvim]], false)
-- Config for: nvim-autopairs
time([[Config for nvim-autopairs]], true)
try_loadstring("\27LJ\2\nG\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\"floppy.configs.nvim-autopairs\frequire\0", "config", "nvim-autopairs")
time([[Config for nvim-autopairs]], false)
-- Config for: vimtex
time([[Config for vimtex]], true)
try_loadstring("\27LJ\2\n?\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\26floppy.configs.vimtex\frequire\0", "config", "vimtex")
time([[Config for vimtex]], false)
-- Config for: bufferline.nvim
time([[Config for bufferline.nvim]], true)
try_loadstring("\27LJ\2\nC\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\30floppy.configs.bufferline\frequire\0", "config", "bufferline.nvim")
time([[Config for bufferline.nvim]], false)
-- Config for: nvim-config-local
time([[Config for nvim-config-local]], true)
try_loadstring("\27LJ\2\nÙ\1\0\0\a\0\f\0\0176\0\0\0'\2\1\0B\0\2\0029\1\2\0005\3\4\0005\4\3\0=\4\5\0036\4\6\0009\4\a\0049\4\b\4'\6\t\0B\4\2\2'\5\n\0'\6\1\0&\4\6\4=\4\v\3D\1\2\0\rhashfile\6/\tdata\fstdpath\afn\bvim\17config_files\1\0\3\vsilent\1\20commands_create\2\24autocommands_create\2\1\3\0\0\15.vimrc.lua\v.vimrc\nsetup\17config-local\frequire\0", "config", "nvim-config-local")
time([[Config for nvim-config-local]], false)
-- Load plugins in order defined by `after`
time([[Sequenced loading]], true)
vim.cmd [[ packadd rust-tools.nvim ]]

-- Config for: rust-tools.nvim
try_loadstring("\27LJ\2\nC\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\30floppy.configs.rust-tools\frequire\0", "config", "rust-tools.nvim")

vim.cmd [[ packadd null-ls.nvim ]]

-- Config for: null-ls.nvim
try_loadstring("\27LJ\2\n@\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\27floppy.configs.null-ls\frequire\0", "config", "null-ls.nvim")

time([[Sequenced loading]], false)
vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
time([[Defining lazy-load filetype autocommands]], true)
vim.cmd [[au FileType org ++once lua require("packer.load")({'orgmode'}, { ft = "org" }, _G.packer_plugins)]]
time([[Defining lazy-load filetype autocommands]], false)
vim.cmd("augroup END")
vim.cmd [[augroup filetypedetect]]
time([[Sourcing ftdetect script at: /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/orgmode/ftdetect/filetype.lua]], true)
vim.cmd [[source /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/orgmode/ftdetect/filetype.lua]]
time([[Sourcing ftdetect script at: /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/orgmode/ftdetect/filetype.lua]], false)
time([[Sourcing ftdetect script at: /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/orgmode/ftdetect/org.vim]], true)
vim.cmd [[source /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/orgmode/ftdetect/org.vim]]
time([[Sourcing ftdetect script at: /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/orgmode/ftdetect/org.vim]], false)
time([[Sourcing ftdetect script at: /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/orgmode/ftdetect/org_archive.vim]], true)
vim.cmd [[source /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/orgmode/ftdetect/org_archive.vim]]
time([[Sourcing ftdetect script at: /Users/danilshvalov/.local/share/nvim/site/pack/packer/opt/orgmode/ftdetect/org_archive.vim]], false)
vim.cmd("augroup END")
if should_profile then save_profiles() end

end)

if not no_errors then
  error_msg = error_msg:gsub('"', '\\"')
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
