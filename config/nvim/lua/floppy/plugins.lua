-- :fennel:1652171198
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
    local function _3_()
      local module_1_auto = require("floppy.configs.yanky")
      return module_1_auto.setup()
    end
    use({config = _3_, "gbprod/yanky.nvim"})
    use({"danilshvalov/dna.nvim"})
    use({after = "nvim-treesitter", config = "floppy.configs.neorg", ft = "norg", requires = "nvim-lua/plenary.nvim", "nvim-neorg/neorg"})
    local function _4_()
      return require("floppy.configs.align")
    end
    use({config = _4_, "junegunn/vim-easy-align"})
    local function _5_()
      local theme = require("github-theme")
      theme.setup({theme_style = "dark"})
      return vim.api.nvim_set_hl(0, "CmpItemKindSnippetDefault", {fg = "#666666"})
    end
    use({config = _5_, "projekt0n/github-nvim-theme"})
    local function _6_()
      local icons = require("nvim-web-devicons")
      return icons.setup({override = {fnl = {icon = "\238\156\170", color = "#fff3d6", name = "Fennel"}}, default = true})
    end
    use({config = _6_, "kyazdani42/nvim-web-devicons"})
    local function _7_()
      local module_1_auto = require("floppy.configs.luasnip")
      return module_1_auto.setup()
    end
    use({config = _7_, requires = {"saadparwaiz1/cmp_luasnip"}, "L3MON4D3/LuaSnip"})
    use({"udayvir-singh/tangerine.nvim"})
    use({"udayvir-singh/hibiscus.nvim"})
    use({"McAuleyPenney/tidy.nvim"})
    local function _8_()
      local orgmode = require("orgmode")
      orgmode.setup_ts_grammar()
      return orgmode.setup({})
    end
    use({config = _8_, ft = {"org"}, "nvim-orgmode/orgmode"})
    local function _9_()
      local module_1_auto = require("lsp_signature")
      return module_1_auto.setup()
    end
    use({config = _9_, "ray-x/lsp_signature.nvim"})
    local function _10_()
      local module_1_auto = require("floppy.configs.leap")
      return module_1_auto.setup()
    end
    use({config = _10_, requires = "tpope/vim-repeat", "ggandor/leap.nvim"})
    local function _11_()
      local module_1_auto = require("neogit")
      return module_1_auto.setup({})
    end
    use({config = _11_, "TimUntersberger/neogit"})
    local function _12_()
      return require("floppy.configs.virt-column")
    end
    use({config = _12_, "lukas-reineke/virt-column.nvim"})
    local function _13_()
      local module_1_auto = require("fidget")
      return module_1_auto.setup()
    end
    use({config = _13_, "j-hui/fidget.nvim"})
    local function _14_()
      local module_1_auto = require("floppy.configs.null-ls")
      return module_1_auto.setup()
    end
    use({after = "nvim-cmp", config = _14_, "jose-elias-alvarez/null-ls.nvim"})
    local function _15_()
      local module_1_auto = require("floppy.configs.diffview")
      return module_1_auto.setup()
    end
    use({config = _15_, "sindrets/diffview.nvim"})
    use({"akinsho/git-conflict.nvim"})
    local function _16_()
      local module_1_auto = require("floppy.configs.lualine")
      return module_1_auto.setup()
    end
    use({after = {"github-nvim-theme"}, config = _16_, requires = {"kyazdani42/nvim-web-devicons"}, "nvim-lualine/lualine.nvim"})
    local function _17_()
      local module_1_auto = require("gitsigns")
      return module_1_auto.setup()
    end
    use({config = _17_, requires = {"nvim-lua/plenary.nvim"}, "lewis6991/gitsigns.nvim"})
    local function _18_()
      local module_1_auto = require("floppy.configs.neo-tree")
      return module_1_auto.setup()
    end
    use({branch = "v2.x", config = _18_, requires = {"nvim-lua/plenary.nvim", "kyazdani42/nvim-web-devicons", "MunifTanjim/nui.nvim"}, "nvim-neo-tree/neo-tree.nvim"})
    local function _19_()
      local module_1_auto = require("floppy.configs.todo-comments")
      return module_1_auto.setup()
    end
    use({config = _19_, requires = "nvim-lua/plenary.nvim", "folke/todo-comments.nvim"})
    use({"famiu/bufdelete.nvim"})
    local function _20_()
      local module_1_auto = require("floppy.configs.move")
      return module_1_auto.setup()
    end
    use({config = _20_, "fedepujol/move.nvim"})
    local function _21_()
      local module_1_auto = require("floppy.configs.vimtex")
      return module_1_auto.setup()
    end
    use({config = _21_, ft = "tex", "lervag/vimtex"})
    local function _22_()
      local module_1_auto = require("floppy.configs.bufferline")
      return module_1_auto.setup()
    end
    use({config = _22_, requires = "kyazdani42/nvim-web-devicons", "akinsho/bufferline.nvim"})
    local function _23_()
      local module_1_auto = require("floppy.configs.nvim-autopairs")
      return module_1_auto.setup()
    end
    use({config = _23_, "windwp/nvim-autopairs"})
    use({"tpope/vim-surround"})
    local function _24_()
      local kommentary = require("kommentary.config")
      return kommentary.configure_language("default", {prefer_single_line_comments = true})
    end
    use({config = _24_, "b3nj5m1n/kommentary"})
    local function _25_()
      local module_1_auto = require("floppy.configs.lspconfig")
      return module_1_auto.setup()
    end
    use({config = _25_, requires = "williamboman/nvim-lsp-installer", "neovim/nvim-lspconfig"})
    local function _26_()
      local module_1_auto = require("floppy.configs.nvim-cmp")
      return module_1_auto.setup()
    end
    use({config = _26_, requires = {"hrsh7th/cmp-nvim-lsp", "hrsh7th/cmp-buffer", "hrsh7th/cmp-path", "hrsh7th/cmp-cmdline"}, "hrsh7th/nvim-cmp"})
    local function _27_()
      local module_1_auto = require("floppy.configs.lsp-colors")
      return module_1_auto.setup()
    end
    use({config = _27_, "folke/lsp-colors.nvim"})
    local function _28_()
      local module_1_auto = require("floppy.configs.lspsaga")
      return module_1_auto.setup()
    end
    use({config = _28_, "tami5/lspsaga.nvim"})
    local function _29_()
      local module_1_auto = require("floppy.configs.telescope")
      return module_1_auto.setup()
    end
    use({config = _29_, requires = {"nvim-lua/popup.nvim", "nvim-lua/plenary.nvim", "nvim-telescope/telescope-project.nvim", "tami5/sqlite.lua"}, "nvim-telescope/telescope.nvim"})
    local function _30_()
      local module_1_auto = require("floppy.configs.nvim-treesitter")
      return module_1_auto.setup()
    end
    use({config = _30_, run = ":TSUpdate", "nvim-treesitter/nvim-treesitter"})
    local function _31_()
      local module_1_auto = require("floppy.configs.lsp-trouble")
      return module_1_auto.setup()
    end
    use({config = _31_, "folke/lsp-trouble.nvim"})
    local function _32_()
      local module_1_auto = require("floppy.configs.which-key")
      return module_1_auto.setup()
    end
    use({config = _32_, "folke/which-key.nvim"})
    use({"kburdett/vim-nuuid"})
    local function _33_()
      return require("floppy.configs.indent-blankline")
    end
    use({config = _33_, "lukas-reineke/indent-blankline.nvim"})
    use({"folke/lua-dev.nvim"})
    use({"dstein64/vim-startuptime"})
    local function _34_()
      return require("impatient")
    end
    use({config = _34_, "lewis6991/impatient.nvim"})
  end
  if _G.packer_bootstrap then
    return (require("packer")).sync()
  else
    return nil
  end
end
do end (require("packer")).startup(_2_)
local p = require("packer")
vim.keymap.set("n", "<Leader>ec", p.compile, {silent = true})
vim.keymap.set("n", "<Leader>ei", p.install, {silent = true})
vim.keymap.set("n", "<Leader>eu", p.update, {silent = true})
return vim.keymap.set("n", "<Leader>es", "<Cmd>so %<CR>", {silent = true})