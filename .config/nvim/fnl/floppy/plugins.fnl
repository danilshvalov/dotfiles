(import-macros {: packer : packer-setup : use!} :hibiscus.packer)
(import-macros {: cfg! : nmap! : datapath!} :floppy.macros)

(packer-setup {})

(packer
    (use! "wbthomason/packer.nvim")

    ;improve vim-yank
    (use!
        :gbprod/yanky.nvim
        :config (cfg! :floppy.configs.yanky))

    ; fennel manager
    (use! :udayvir-singh/tangerine.nvim)

    ; fennel macros
    (use! :udayvir-singh/hibiscus.nvim)

    ; remove trailing space
    (use! :McAuleyPenney/tidy.nvim)

    (use!
        "nvim-orgmode/orgmode"
        :ft [:org]
        :config (fn []
            (local orgmode (require :orgmode))
            (orgmode.setup_ts_grammar)
            (orgmode.setup {})
        )
    )

    (use!
        "ray-x/lsp_signature.nvim"
        :config (cfg! :lsp_signature)
    )

    (use!
        "ggandor/leap.nvim"
        :requires "tpope/vim-repeat"
        :config (cfg! "floppy.configs.leap")
    )

    (use!
        "klen/nvim-config-local"
        :config (cfg! "config-local"
            {
                :config_files  [ ".vimrc.lua" ".vimrc" ]
                :hashfile  (datapath! :config-local)
                :autocommands_create  true
                :commands_create  true
                :silent  false
            }
        )
    )

    (use!
        "https://github.com/TimUntersberger/neogit"
        :config (cfg! "neogit" {})
    )

    (use!
        "lukas-reineke/virt-column.nvim"
        :config (cfg! :floppy.configs.virt-column)
    )

    (use!
        "j-hui/fidget.nvim"
        :config (cfg! "fidget")
    )

    (use!
        "jose-elias-alvarez/null-ls.nvim"
        :config (cfg! "floppy.configs.null-ls")
        :after "nvim-cmp"
    )

    (use!
        "folke/tokyonight.nvim"
        :config (fn []
            vim.o.background  "dark"
            (vim.cmd "colorscheme tokyonight")
        )
    )

    (use!
        "sindrets/diffview.nvim"
        :config (cfg! "floppy.configs.diffview")
    )

    (use!
        "rhysd/conflict-marker.vim"
        :config (cfg! "floppy.configs.conflict-marker")
    )

    (use!
        "nvim-lualine/lualine.nvim"
        :requires [ "kyazdani42/nvim-web-devicons" ]
        :config (cfg! "floppy.configs.lualine")
    )

    (use!
        "lewis6991/gitsigns.nvim"
        :requires [ "nvim-lua/plenary.nvim" ]
        :config (cfg! "gitsigns")
    )

    (use!
        "nvim-neo-tree/neo-tree.nvim"
        :branch "v2.x"
        :requires [
            "nvim-lua/plenary.nvim"
            "kyazdani42/nvim-web-devicons"
            "MunifTanjim/nui.nvim"
        ]
        :config (cfg! "floppy.configs.neo-tree")
    )

    (use!
        "folke/todo-comments.nvim"
        :requires "nvim-lua/plenary.nvim"
        :config (cfg! "floppy.configs.todo-comments")
    )

    (use! "famiu/bufdelete.nvim")

    (use!
        "simrat39/rust-tools.nvim"
        :after "nvim-lspconfig"
        :config (cfg! "floppy.configs.rust-tools")
    )

    (use!
        "fedepujol/move.nvim"
        :config (cfg! "floppy.configs.move")
    )

    (use! "jose-elias-alvarez/nvim-lsp-ts-utils")

    (use!
        "lervag/vimtex"
        :config (cfg! "floppy.configs.vimtex")
    )

    (use! "nvim-lua/plenary.nvim")

    (use!
        "dcampos/nvim-snippy"
        :requires "dcampos/cmp-snippy"
        :config (cfg! "floppy.configs.snippy")
    )

    (use! "folke/lua-dev.nvim")

    (use!
        "akinsho/bufferline.nvim"
        :requires "kyazdani42/nvim-web-devicons"
        :config (cfg! "floppy.configs.bufferline")
    )

    (use!
        "lukas-reineke/indent-blankline.nvim"
        :config (cfg! "floppy.configs.indent-blankline")
    )

    (use!
        "windwp/nvim-autopairs"
        :config (cfg! "floppy.configs.nvim-autopairs")
    )

    (use! "tpope/vim-surround")

    (use!
        "b3nj5m1n/kommentary"
        :config (fn []
            (let [kommentary (require "kommentary.config")]
                (kommentary.configure_language :default {:prefer_single_line_comments true})
            )
        )
    )

    (use!
        "neovim/nvim-lspconfig"
        :requires "williamboman/nvim-lsp-installer"
        :config (cfg! "floppy.configs.lspconfig")
    )

    (use!
        "hrsh7th/nvim-cmp"
        :config (cfg! "floppy.configs.nvim-cmp")
        :requires [
            "hrsh7th/cmp-nvim-lsp"
            "hrsh7th/cmp-buffer"
            "hrsh7th/cmp-path"
            "hrsh7th/cmp-cmdline"
        ]
    )

    (use!
        "folke/lsp-colors.nvim"
        :config (cfg! "floppy.configs.lsp-colors")
    )

    (use!
        "tami5/lspsaga.nvim"
        :config (cfg! "floppy.configs.lspsaga")
    )

    (use!
        "nvim-telescope/telescope.nvim"
        :requires  [
             "nvim-lua/popup.nvim"
             "nvim-lua/plenary.nvim"
             "nvim-telescope/telescope-project.nvim"
             "tami5/sqlite.lua"
        ]
        :config (cfg! "floppy.configs.telescope")
    )

    (use!
        "nvim-treesitter/nvim-treesitter"
        :run ":TSUpdate"
        :config (cfg! "floppy.configs.nvim-treesitter")
    )

    (use!
        "folke/lsp-trouble.nvim"
        :config (cfg! "floppy.configs.lsp-trouble")
    )

    (use!
        "folke/which-key.nvim"
        :config (cfg! "floppy.configs.which-key")
    )
)

(nmap! "<Leader>ec" "<Cmd>PackerCompile<CR>")
(nmap!"<Leader>ei" "<Cmd>PackerInstall<CR>")
(nmap! "<Leader>es" "<Cmd>so %<CR>")
