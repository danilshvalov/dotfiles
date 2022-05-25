-- :fennel:1653324957
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
      local theme = require("github-theme")
      theme.setup({theme_style = "dark_default"})
      vim.cmd("colorscheme github_dark_default")
      vim.api.nvim_set_hl(0, "CmpItemKindSnippetDefault", {fg = "#666666"})
      vim.api.nvim_set_hl(0, "cppTSProperty", {fg = "#e1e4e8"})
      vim.api.nvim_set_hl(0, "cppTSType", {fg = "#75beff"})
      return vim.api.nvim_set_hl(0, "VertSplit", {fg = "#505050"})
    end
    use({config = _3_, "projekt0n/github-nvim-theme"})
    use({run = "make", "nvim-telescope/telescope-fzf-native.nvim"})
    local function _4_()
      local marks = require("marks")
      return marks.setup()
    end
    use({config = _4_, "chentoast/marks.nvim"})
    local function _5_()
      return require("vdk.plugins.configs.reach")
    end
    use({config = _5_, "toppair/reach.nvim"})
    local function _6_()
      local module_1_auto = require("floppy.configs.yanky")
      return module_1_auto.setup()
    end
    use({config = _6_, "gbprod/yanky.nvim"})
    use({"danilshvalov/dna.nvim"})
    use({"TimUntersberger/neogit"})
    local function _7_()
      return require("floppy.configs.neorg")
    end
    use({after = "nvim-treesitter", config = _7_, ft = "norg", requires = "nvim-lua/plenary.nvim", "nvim-neorg/neorg"})
    local function _8_()
      return require("vdk.plugins.configs.align")
    end
    use({config = _8_, "junegunn/vim-easy-align"})
    local function _9_()
      local icons = require("nvim-web-devicons")
      return icons.setup({override = {fnl = {icon = "\238\156\170", color = "#fff3d6", name = "Fennel"}}, default = true})
    end
    use({config = _9_, "kyazdani42/nvim-web-devicons"})
    local function _10_()
      local module_1_auto = require("floppy.configs.luasnip")
      return module_1_auto.setup()
    end
    use({config = _10_, requires = {"saadparwaiz1/cmp_luasnip"}, "L3MON4D3/LuaSnip"})
    use({"udayvir-singh/tangerine.nvim"})
    use({"udayvir-singh/hibiscus.nvim"})
    use({"McAuleyPenney/tidy.nvim"})
    local function _11_()
      return require("vdk.plugins.configs.lsp-signature")
    end
    use({config = _11_, "ray-x/lsp_signature.nvim"})
    local function _12_()
      return require("vdk.plugins.configs.leap")
    end
    use({config = _12_, requires = "tpope/vim-repeat", "ggandor/leap.nvim"})
    local function _13_()
      return require("floppy.configs.virt-column")
    end
    use({config = _13_, "lukas-reineke/virt-column.nvim"})
    local function _14_()
      local module_1_auto = require("fidget")
      return module_1_auto.setup()
    end
    use({config = _14_, "j-hui/fidget.nvim"})
    local function _15_()
      local module_1_auto = require("floppy.configs.null-ls")
      return module_1_auto.setup()
    end
    use({after = "nvim-cmp", config = _15_, "jose-elias-alvarez/null-ls.nvim"})
    local function _16_()
      local module_1_auto = require("floppy.configs.diffview")
      return module_1_auto.setup()
    end
    use({config = _16_, "sindrets/diffview.nvim"})
    use({"akinsho/git-conflict.nvim"})
    local function _17_()
      local module_1_auto = require("floppy.configs.lualine")
      return module_1_auto.setup()
    end
    use({after = {"github-nvim-theme"}, config = _17_, requires = {"kyazdani42/nvim-web-devicons"}, "nvim-lualine/lualine.nvim"})
    local function _18_()
      local module_1_auto = require("gitsigns")
      return module_1_auto.setup()
    end
    use({config = _18_, requires = {"nvim-lua/plenary.nvim"}, "lewis6991/gitsigns.nvim"})
    local function _19_()
      local module_1_auto = require("floppy.configs.neo-tree")
      return module_1_auto.setup()
    end
    use({branch = "v2.x", config = _19_, requires = {"nvim-lua/plenary.nvim", "kyazdani42/nvim-web-devicons", "MunifTanjim/nui.nvim"}, "nvim-neo-tree/neo-tree.nvim"})
    local function _20_()
      local module_1_auto = require("floppy.configs.todo-comments")
      return module_1_auto.setup()
    end
    use({config = _20_, requires = "nvim-lua/plenary.nvim", "folke/todo-comments.nvim"})
    use({"famiu/bufdelete.nvim"})
    local function _21_()
      local module_1_auto = require("floppy.configs.move")
      return module_1_auto.setup()
    end
    use({config = _21_, "fedepujol/move.nvim"})
    local function _22_()
      local module_1_auto = require("floppy.configs.vimtex")
      return module_1_auto.setup()
    end
    use({config = _22_, ft = "tex", "lervag/vimtex"})
    local function _23_()
      local module_1_auto = require("floppy.configs.nvim-autopairs")
      return module_1_auto.setup()
    end
    use({config = _23_, "windwp/nvim-autopairs"})
    use({"tpope/vim-surround"})
    local function _24_()
      local Comment = require("Comment")
      return Comment.setup()
    end
    use({config = _24_, "numToStr/Comment.nvim"})
    local function _25_()
      return require("vdk.plugins.configs.lspconfig")
    end
    use({config = _25_, requires = "williamboman/nvim-lsp-installer", "neovim/nvim-lspconfig"})
    local function _26_()
      local module_1_auto = require("floppy.configs.nvim-cmp")
      return module_1_auto.setup()
    end
    use({config = _26_, requires = {"hrsh7th/cmp-nvim-lsp", "hrsh7th/cmp-buffer", "hrsh7th/cmp-path", "hrsh7th/cmp-cmdline"}, "hrsh7th/nvim-cmp"})
    local function _27_()
      return require("vdk.plugins.configs.lsp-colors")
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
      return require("vdk.plugins.configs.indent-blankline")
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