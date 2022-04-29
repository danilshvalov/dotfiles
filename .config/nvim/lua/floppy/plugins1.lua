local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
    execute("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
    execute("packadd packer.nvim")
end

local use = require("packer").use
local packer = require("packer").startup(function()
    use("wbthomason/packer.nvim")

    use("McAuleyPenney/tidy.nvim")

    use("kburdett/vim-nuuid")

    -- use({
    --     "catppuccin/nvim",
    --     as = "catppuccin",
    --     config = function()
    --         local catppuccin = require("catppuccin")
    --         catppuccin.remap({ Comment = { fg = "#5bd46b" } })
    --         vim.cmd("colorscheme catppuccin")
    --     end,
    -- })

    use({
        "gbprod/yanky.nvim",
        config = function()
            require("floppy.configs.yanky").setup()
        end,
    })

    use({ "udayvir-singh/tangerine.nvim" })

    use({
        "nvim-orgmode/orgmode",
        ft = { "org" },
        config = function()
            require("orgmode").setup_ts_grammar()
            require("orgmode").setup({})
        end,
    })

    use({
        "ray-x/lsp_signature.nvim",
        config = function()
            require("lsp_signature").setup()
        end,
    })

    use({
        "nmac427/guess-indent.nvim",
        config = function()
            require("guess-indent").setup({})
        end,
    })

    use({
        "ggandor/leap.nvim",
        requires = "tpope/vim-repeat",
        config = function()
            require("floppy.configs.leap").setup()
        end,
    })

    use({
        "klen/nvim-config-local",
        config = function()
            require("config-local").setup({
                config_files = { ".vimrc.lua", ".vimrc" },
                hashfile = vim.fn.stdpath("data") .. "/config-local",
                autocommands_create = true,
                commands_create = true,
                silent = false,
            })
        end,
    })

    use({
        "https://github.com/TimUntersberger/neogit",
        config = function()
            local neogit = require("neogit")
            neogit.setup({})
        end,
    })

    use({
        "untitled-ai/jupyter_ascending.vim",
        config = function()
            vim.keymap.set("", "<Leader>e", "<Plug>JupyterExecute")
        end,
    })

    use("iamcco/markdown-preview.nvim")

    use({
        "goerz/jupytext.vim",
        config = function()
            vim.g.jupytext_fmt = "py:percent"
        end,
    })

    use({
        "lukas-reineke/virt-column.nvim",
        config = function()
            require("floppy.configs.virt-column").setup()
        end,
    })

    use({
        "j-hui/fidget.nvim",
        config = function()
            require("fidget").setup({})
        end,
    })

    use({
        "jose-elias-alvarez/null-ls.nvim",
        config = function()
            require("floppy.configs.null-ls").setup()
        end,
        after = "nvim-cmp",
    })

    use({
        "folke/tokyonight.nvim",
        config = function()
            vim.o.background = "dark"
            vim.cmd("colorscheme tokyonight")
            vim.cmd("hi TSComment guifg=green")
        end,
    })

    use({
        "sindrets/diffview.nvim",
        config = function()
            require("floppy.configs.diffview")
        end,
    })

    use({
        "rhysd/conflict-marker.vim",
        config = function()
            require("floppy.configs.conflict-marker")
        end,
    })

    use({
        "nvim-lualine/lualine.nvim",
        requires = { "kyazdani42/nvim-web-devicons", opt = true },
        config = function()
            require("floppy.configs.lualine").setup()
        end,
    })

    use({
        "lewis6991/gitsigns.nvim",
        requires = {
            "nvim-lua/plenary.nvim",
        },
        config = function()
            require("gitsigns").setup()
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
            require("floppy.configs.neo-tree").setup()
        end,
    })

    use({
        "folke/todo-comments.nvim",
        requires = "nvim-lua/plenary.nvim",
        config = function()
            require("floppy.configs.todo-comments")
        end,
    })

    use({
        "abecodes/tabout.nvim",
        config = function()
            require("floppy.configs.tabout")
        end,
    })

    use("famiu/bufdelete.nvim")

    use({
        "simrat39/rust-tools.nvim",
        config = function()
            require("floppy.configs.rust-tools")
        end,
        after = "nvim-lspconfig",
    })

    use({
        "fedepujol/move.nvim",
        config = function()
            require("floppy.configs.move").setup()
        end,
    })

    use("jose-elias-alvarez/nvim-lsp-ts-utils")

    use({
        "lervag/vimtex",
        config = function()
            require("floppy.configs.vimtex")
        end,
    })

    use({ "nvim-lua/plenary.nvim" })

    use({
        "dcampos/nvim-snippy",
        config = function()
            require("floppy.configs.snippy")
        end,
        requires = "dcampos/cmp-snippy",
    })

    use("folke/lua-dev.nvim")

    use({
        "akinsho/bufferline.nvim",
        requires = "kyazdani42/nvim-web-devicons",
        config = function()
            require("floppy.configs.bufferline")
        end,
    })

    use({
        "lukas-reineke/indent-blankline.nvim",
        config = function()
            require("floppy.configs.indent-blankline")
        end,
    })

    use({
        "windwp/nvim-autopairs",
        config = function()
            require("floppy.configs.nvim-autopairs")
        end,
    })

    use("tpope/vim-surround")

    use({
        "b3nj5m1n/kommentary",
        config = function()
            require("kommentary.config").configure_language("default", {
                prefer_single_line_comments = true,
            })
        end,
    })

    use({
        "neovim/nvim-lspconfig",
        config = function()
            require("floppy.configs.lspconfig").setup()
        end,
    })

    use({
        "williamboman/nvim-lsp-installer",
        config = function()
            require("floppy.configs.lsp-install")
        end,
    })

    use({
        "hrsh7th/nvim-cmp",
        config = function()
            require("floppy.configs.nvim-cmp")
        end,
        requires = {
            "hrsh7th/cmp-nvim-lsp",
            "hrsh7th/cmp-buffer",
            "hrsh7th/cmp-path",
            "hrsh7th/cmp-cmdline",
        },
    })

    use({
        "folke/lsp-colors.nvim",
        config = function()
            require("floppy.configs.lsp-colors")
        end,
    })

    use({
        "tami5/lspsaga.nvim",
        config = function()
            require("floppy.configs.lspsaga").setup()
        end,
    })

    use({
        "nvim-telescope/telescope.nvim",
        requires = {
            { "nvim-lua/popup.nvim" },
            { "nvim-lua/plenary.nvim" },
            { "nvim-telescope/telescope-project.nvim" },
            { "tami5/sqlite.lua" },
        },
        config = function()
            require("floppy.configs.telescope")
        end,
    })

    use({
        "nvim-treesitter/nvim-treesitter",
        run = ":TSUpdate",
        config = function()
            require("floppy.configs.nvim-treesitter")
        end,
    })

    use({
        "folke/lsp-trouble.nvim",
        config = function()
            require("floppy.configs.lsp-trouble")
        end,
    })

    use({
        "folke/which-key.nvim",
        config = function()
            require("floppy.configs.which-key")
        end,
    })
end)

local map = require("floppy.utils.mappings").map
map("<Leader>ec", "<Cmd>PackerCompile<CR>")
map("<Leader>ei", "<Cmd>PackerInstall<CR>")
map("<Leader>es", "<Cmd>so %<CR>")

return packer
