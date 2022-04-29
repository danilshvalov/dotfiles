-- :fennel:1651221107
do
  if (0 == vim.fn.isdirectory("/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/packer.nvim")) then
    print("packer.nvim: installing in data dir...")
    do end (_G)["packer_bootstrap"] = vim.fn.system({"git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", "/Users/danilshvalov/.local/share/nvim/site/pack/packer/start/packer.nvim"})
    vim.cmd("redraw")
    vim.cmd("packadd packer.nvim")
    print("packer.nvim: installed")
  else
  end
  do end (require("packer")).init({})
end
local function _2_(use)
  _G.assert((nil ~= use), "Missing argument use on floppy/plugins.fnl:6")
  use("wbthomason/packer.nvim")
  do
    use({"wbthomason/packer.nvim"})
    local function _3_()
      local module_2_auto = require("floppy.configs.yanky")
      return module_2_auto.setup()
    end
    use({config = _3_, "gbprod/yanky.nvim"})
    use({"udayvir-singh/tangerine.nvim"})
    use({"udayvir-singh/hibiscus.nvim"})
    use({"McAuleyPenney/tidy.nvim"})
    local function _4_()
      local orgmode = require("orgmode")
      orgmode.setup_ts_grammar()
      return orgmode.setup({})
    end
    use({config = _4_, ft = {"org"}, "nvim-orgmode/orgmode"})
    local function _5_()
      local module_2_auto = require("lsp_signature")
      return module_2_auto.setup()
    end
    use({config = _5_, "ray-x/lsp_signature.nvim"})
    local function _6_()
      local module_2_auto = require("floppy.configs.leap")
      return module_2_auto.setup()
    end
    use({config = _6_, requires = "tpope/vim-repeat", "ggandor/leap.nvim"})
    local function _7_()
      local module_2_auto = require("config-local")
      return module_2_auto.setup({config_files = {".vimrc.lua", ".vimrc"}, hashfile = (vim.fn.stdpath("data") .. "/" .. "config-local"), autocommands_create = true, commands_create = true, silent = false})
    end
    use({config = _7_, "klen/nvim-config-local"})
    local function _8_()
      local module_2_auto = require("neogit")
      return module_2_auto.setup({})
    end
    use({config = _8_, "https://github.com/TimUntersberger/neogit"})
    local function _9_()
      local module_2_auto = require("floppy.configs.virt-column")
      return module_2_auto.setup()
    end
    use({config = _9_, "lukas-reineke/virt-column.nvim"})
    local function _10_()
      local module_2_auto = require("fidget")
      return module_2_auto.setup()
    end
    use({config = _10_, "j-hui/fidget.nvim"})
    local function _11_()
      local module_2_auto = require("floppy.configs.null-ls")
      return module_2_auto.setup()
    end
    use({after = "nvim-cmp", config = _11_, "jose-elias-alvarez/null-ls.nvim"})
    local function _12_()
      do local _ = vim.o.background end
      return vim.cmd("colorscheme tokyonight")
    end
    use({config = _12_, "folke/tokyonight.nvim"})
    local function _13_()
      local module_2_auto = require("floppy.configs.diffview")
      return module_2_auto.setup()
    end
    use({config = _13_, "sindrets/diffview.nvim"})
    local function _14_()
      local module_2_auto = require("floppy.configs.conflict-marker")
      return module_2_auto.setup()
    end
    use({config = _14_, "rhysd/conflict-marker.vim"})
    local function _15_()
      local module_2_auto = require("floppy.configs.lualine")
      return module_2_auto.setup()
    end
    use({config = _15_, requires = {"kyazdani42/nvim-web-devicons"}, "nvim-lualine/lualine.nvim"})
    local function _16_()
      local module_2_auto = require("gitsigns")
      return module_2_auto.setup()
    end
    use({config = _16_, requires = {"nvim-lua/plenary.nvim"}, "lewis6991/gitsigns.nvim"})
    local function _17_()
      local module_2_auto = require("floppy.configs.neo-tree")
      return module_2_auto.setup()
    end
    use({branch = "v2.x", config = _17_, requires = {"nvim-lua/plenary.nvim", "kyazdani42/nvim-web-devicons", "MunifTanjim/nui.nvim"}, "nvim-neo-tree/neo-tree.nvim"})
    local function _18_()
      local module_2_auto = require("floppy.configs.todo-comments")
      return module_2_auto.setup()
    end
    use({config = _18_, requires = "nvim-lua/plenary.nvim", "folke/todo-comments.nvim"})
    use({"famiu/bufdelete.nvim"})
    local function _19_()
      local module_2_auto = require("floppy.configs.rust-tools")
      return module_2_auto.setup()
    end
    use({after = "nvim-lspconfig", config = _19_, "simrat39/rust-tools.nvim"})
    local function _20_()
      local module_2_auto = require("floppy.configs.move")
      return module_2_auto.setup()
    end
    use({config = _20_, "fedepujol/move.nvim"})
    use({"jose-elias-alvarez/nvim-lsp-ts-utils"})
    local function _21_()
      local module_2_auto = require("floppy.configs.vimtex")
      return module_2_auto.setup()
    end
    use({config = _21_, "lervag/vimtex"})
    use({"nvim-lua/plenary.nvim"})
    local function _22_()
      local module_2_auto = require("floppy.configs.snippy")
      return module_2_auto.setup()
    end
    use({config = _22_, requires = "dcampos/cmp-snippy", "dcampos/nvim-snippy"})
    use({"folke/lua-dev.nvim"})
    local function _23_()
      local module_2_auto = require("floppy.configs.bufferline")
      return module_2_auto.setup()
    end
    use({config = _23_, requires = "kyazdani42/nvim-web-devicons", "akinsho/bufferline.nvim"})
    local function _24_()
      local module_2_auto = require("floppy.configs.indent-blankline")
      return module_2_auto.setup()
    end
    use({config = _24_, "lukas-reineke/indent-blankline.nvim"})
    local function _25_()
      local module_2_auto = require("floppy.configs.nvim-autopairs")
      return module_2_auto.setup()
    end
    use({config = _25_, "windwp/nvim-autopairs"})
    use({"tpope/vim-surround"})
    local function _26_()
      local kommentary = require("kommentary.config")
      return kommentary.configure_language("default", {prefer_single_line_comments = true})
    end
    use({config = _26_, "b3nj5m1n/kommentary"})
    local function _27_()
      local module_2_auto = require("floppy.configs.lspconfig")
      return module_2_auto.setup()
    end
    use({config = _27_, requires = "williamboman/nvim-lsp-installer", "neovim/nvim-lspconfig"})
    local function _28_()
      local module_2_auto = require("floppy.configs.nvim-cmp")
      return module_2_auto.setup()
    end
    use({config = _28_, requires = {"hrsh7th/cmp-nvim-lsp", "hrsh7th/cmp-buffer", "hrsh7th/cmp-path", "hrsh7th/cmp-cmdline"}, "hrsh7th/nvim-cmp"})
    local function _29_()
      local module_2_auto = require("floppy.configs.lsp-colors")
      return module_2_auto.setup()
    end
    use({config = _29_, "folke/lsp-colors.nvim"})
    local function _30_()
      local module_2_auto = require("floppy.configs.lspsaga")
      return module_2_auto.setup()
    end
    use({config = _30_, "tami5/lspsaga.nvim"})
    local function _31_()
      local module_2_auto = require("floppy.configs.telescope")
      return module_2_auto.setup()
    end
    use({config = _31_, requires = {"nvim-lua/popup.nvim", "nvim-lua/plenary.nvim", "nvim-telescope/telescope-project.nvim", "tami5/sqlite.lua"}, "nvim-telescope/telescope.nvim"})
    local function _32_()
      local module_2_auto = require("floppy.configs.nvim-treesitter")
      return module_2_auto.setup()
    end
    use({config = _32_, run = ":TSUpdate", "nvim-treesitter/nvim-treesitter"})
    local function _33_()
      local module_2_auto = require("floppy.configs.lsp-trouble")
      return module_2_auto.setup()
    end
    use({config = _33_, "folke/lsp-trouble.nvim"})
    local function _34_()
      local module_2_auto = require("floppy.configs.which-key")
      return module_2_auto.setup()
    end
    use({config = _34_, "folke/which-key.nvim"})
  end
  if _G.packer_bootstrap then
    return (require("packer")).sync()
  else
    return nil
  end
end
do end (require("packer")).startup(_2_)
vim.keymap.set("n", "<Leader>ec", "<Cmd>PackerCompile<CR>", {silent = true})
vim.keymap.set("n", "<Leader>ei", "<Cmd>PackerInstall<CR>", {silent = true})
return vim.keymap.set("n", "<Leader>es", "<Cmd>so %<CR>", {silent = true})