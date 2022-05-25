local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({
        "git",
        "clone",
        "--depth",
        "1",
        "https://github.com/wbthomason/packer.nvim",
        install_path,
    })
end

require("packer").startup(function(use)
    use({ "wbthomason/packer.nvim" })

    use({ "lewis6991/impatient.nvim" })

    use({
        "danymat/neogen",
        requires = "nvim-treesitter/nvim-treesitter",
        config = function()
            require("vdk.plugins.configs.neogen")
        end,
    })

    use({ "dstein64/vim-startuptime" })

    use({
        "lukas-reineke/indent-blankline.nvim",
        config = function()
            require("vdk.plugins.configs.indent-blankline")
        end,
    })

    use({
        "nvim-neo-tree/neo-tree.nvim",
        branch = "v2.x",
        requires = {
            "nvim-lua/plenary.nvim",
            "kyazdani42/nvim-web-devicons",
            "MunifTanjim/nui.nvim",
        },
        config = function()
            require("vdk.plugins.configs.neo-tree")
        end,
    })

    use({
        "windwp/nvim-autopairs",
        config = function()
            require("vdk.plugins.configs.nvim-autopairs")
        end,
    })

    use({
        "fedepujol/move.nvim",
        -- cfg = "move",
    })

    use({
        "sindrets/diffview.nvim",
        -- cfg = "diffview",
    })

    use({
        "gbprod/yanky.nvim",
        config = function()
            require("vdk.plugins.configs.yanky")
        end,
    })

    use({
        "folke/todo-comments.nvim",
        requires = "nvim-lua/plenary.nvim",
        -- cfg = "todo-comments",
    })

    use({
        "numToStr/Comment.nvim",
        config = function()
            require("vdk.plugins.configs.comment")
        end,
    })

    use({
        "lukas-reineke/virt-column.nvim",
        config = function()
            require("vdk.plugins.configs.virt-column")
        end,
    })

    use({
        -- FIX: revert
        -- "nvim-lualine/lualine.nvim",
        "diegodox/lualine.nvim",
        branch = "winbar",
        config = function()
            require("vdk.plugins.configs.lualine")
        end,
    })

    use({
        "lervag/vimtex",
        ft = "tex",
        config = function()
            require("vdk.plugins.configs.vimtex")
        end,
    })

    use({ "famiu/bufdelete.nvim" })

    use({ "McAuleyPenney/tidy.nvim" })

    use({
        "L3MON4D3/LuaSnip",
        requires = "saadparwaiz1/cmp_luasnip",
        config = function()
            require("vdk.plugins.configs.luasnip")
        end,
    })

    use({
        "nvim-neorg/neorg",
        requires = "nvim-lua/plenary.nvim",
        ft = "norg",
        after = "nvim-treesitter",
        config = function()
            require("vdk.plugins.configs.neorg")
        end,
    })

    use({
        "ggandor/leap.nvim",
        requires = "tpope/vim-repeat",
        config = function()
            require("vdk.plugins.configs.leap")
        end,
    })

    use({
        "neovim/nvim-lspconfig",
        requires = "williamboman/nvim-lsp-installer",
        config = function()
            require("vdk.plugins.configs.lspconfig")
        end,
    })

    use({
        "tami5/lspsaga.nvim",
        config = function()
            require("vdk.plugins.configs.lspsaga")
        end,
    })

    use({
        "ray-x/lsp_signature.nvim",
        config = function()
            require("vdk.plugins.configs.lsp-signature")
        end,
    })

    use({
        "folke/lsp-colors.nvim",
        config = function()
            require("vdk.plugins.configs.lsp-colors")
        end,
    })

    use({
        "jose-elias-alvarez/null-ls.nvim",
        after = "nvim-cmp",
        config = function()
            require("vdk.plugins.configs.null-ls")
        end,
    })

    use({
        "nvim-telescope/telescope.nvim",
        requires = {
            "nvim-lua/popup.nvim",
            "nvim-lua/plenary.nvim",
            "nvim-telescope/telescope-project.nvim",
            "tami5/sqlite.lua",
            { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
        },
        config = function()
            require("vdk.plugins.configs.telescope")
        end,
    })

    use({
        "nvim-treesitter/nvim-treesitter",
        run = ":TSUpdate",
        config = function()
            require("vdk.plugins.configs.nvim-treesitter")
        end,
    })

    use({
        "projekt0n/github-nvim-theme",
        config = function()
            require("vdk.plugins.configs.github-theme")
        end,
    })

    use({
        "hrsh7th/nvim-cmp",
        requires = {
            "hrsh7th/cmp-nvim-lsp",
            "hrsh7th/cmp-buffer",
            "hrsh7th/cmp-path",
            "hrsh7th/cmp-cmdline",
        },
        config = function()
            require("vdk.plugins.configs.nvim-cmp")
        end,
    })

    use({
        "toppair/reach.nvim",
        config = function()
            require("vdk.plugins.configs.reach")
        end,
    })

    use({
        "junegunn/vim-easy-align",
        config = function()
            require("vdk.plugins.configs.align")
        end,
    })

    use({
        "lewis6991/gitsigns.nvim",
        requires = "nvim-lua/plenary.nvim",
        config = function()
            require("vdk.plugins.configs.gitsigns")
        end,
    })

    use({
        "folke/which-key.nvim",
        config = function()
            local wk = require("which-key")
            wk.setup()
        end,
    })

    use({ "TimUntersberger/neogit" })

    use({ "akinsho/git-conflict.nvim" })

    use({ "kburdett/vim-nuuid" })

    --   (use!
    --       "folke/lsp-trouble.nvim"
    --       :config (-- cfg! "floppy.configs.lsp-trouble")
    --   )
end)

local p = require("packer")
local nmap = require("vdk.core.utils").nmap

nmap("<Leader>ec", p.compile)
nmap("<Leader>ei", p.install)
nmap("<Leader>eu", p.update)
nmap("<Leader>op", function()
    local path = vim.fn.stdpath("config") .. "/lua/vdk/plugins/init.lua"
    vim.cmd("e " .. path)
end)
nmap("<Leader>es", "<Cmd>so %<CR>")
