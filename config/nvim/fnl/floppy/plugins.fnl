(import-macros {: nmap! : datapath! : hi!} :dna.vim)
(import-macros {: packer : packer-setup : use! : cfg!} :dna.plug)

(packer-setup {})

(packer
  (use!
    :projekt0n/github-nvim-theme
    :config (fn []
      (local theme (require :github-theme))
      (theme.setup {
        :theme_style :dark_default
      })
      (vim.cmd "colorscheme github_dark_default")
      (hi! CmpItemKindSnippetDefault {:fg :#666666})
      (hi! cppTSProperty {:fg :#e1e4e8})
      (hi! cppTSType  {:fg :#75beff})
      (hi! VertSplit {:fg :#505050})
    )
  )

  (use!
    :nvim-telescope/telescope-fzf-native.nvim
    :run :make
  )

  (use!
    :chentoast/marks.nvim
    :config (fn []
      (local marks (require :marks))
      (marks.setup)
    )
  )

  (use!
    :toppair/reach.nvim
    :cfg! :vdk.plugins.configs.reach
  )

  ;improve vim-yank
  (use!
    :gbprod/yanky.nvim
    :config (cfg! :floppy.configs.yanky))

  (use! :danilshvalov/dna.nvim)

  (use! :TimUntersberger/neogit)

  (use!
    :nvim-neorg/neorg
    :requires :nvim-lua/plenary.nvim
    :ft :norg
    :after :nvim-treesitter
    :cfg! :floppy.configs.neorg
  )

  (use!
    :junegunn/vim-easy-align
    :cfg! :vdk.plugins.configs.align
  )

  (use!
    :kyazdani42/nvim-web-devicons
    :config (fn []
      (local icons (require :nvim-web-devicons))
      (icons.setup {
        :override {
          :fnl {
            :icon ""
            :color :#fff3d6
            :name "Fennel"
          }
        }
       :default true
      })
    )
  )

  (use!
    :L3MON4D3/LuaSnip
    :requires [:saadparwaiz1/cmp_luasnip]
    :config! :floppy.configs.luasnip
  )

  ; fennel manager
  (use! :udayvir-singh/tangerine.nvim)

  ; fennel macros
  (use! :udayvir-singh/hibiscus.nvim)

  ; remove trailing space
  (use! :McAuleyPenney/tidy.nvim)

  (use!
      :ray-x/lsp_signature.nvim
      :cfg! "vdk.plugins.configs.lsp-signature"
  )

  (use!
    :ggandor/leap.nvim
    :requires "tpope/vim-repeat"
    :cfg! "vdk.plugins.configs.leap"
  )

  (use!
    "lukas-reineke/virt-column.nvim"
    :cfg! :floppy.configs.virt-column
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
      "sindrets/diffview.nvim"
      :config (cfg! "floppy.configs.diffview")
  )

  (use! :akinsho/git-conflict.nvim)

  (use!
      "nvim-lualine/lualine.nvim"
      :requires [ "kyazdani42/nvim-web-devicons" ]
      :config (cfg! "floppy.configs.lualine")
      :after [:github-nvim-theme]
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
      "fedepujol/move.nvim"
      :config (cfg! "floppy.configs.move")
  )

  (use!
      "lervag/vimtex"
      :ft :tex
      :config (cfg! "floppy.configs.vimtex")
  )

  (use!
      "windwp/nvim-autopairs"
      :config (cfg! "floppy.configs.nvim-autopairs")
  )

  (use! "tpope/vim-surround")

  (use!
    :numToStr/Comment.nvim
    :config (fn []
      (local Comment (require :Comment))
      (Comment.setup)
    )
  )

  (use!
      "neovim/nvim-lspconfig"
      :requires "williamboman/nvim-lsp-installer"
      :cfg! "vdk.plugins.configs.lspconfig"
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
      :cfg! "vdk.plugins.configs.lsp-colors"
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

  (use! :kburdett/vim-nuuid)

  (use!
    "lukas-reineke/indent-blankline.nvim"
    :cfg! :vdk.plugins.configs.indent-blankline
  )

  (use! "folke/lua-dev.nvim")

  (use!
    :dstein64/vim-startuptime
  )

  ; improve startup time
  (use!
    :lewis6991/impatient.nvim
    :config (fn [] (require :impatient))
  )
)

(local p (require :packer))

(nmap! "<Leader>ec" p.compile)
(nmap!"<Leader>ei" p.install)
(nmap!"<Leader>eu" p.update)
(nmap! "<Leader>es" "<Cmd>so %<CR>")
