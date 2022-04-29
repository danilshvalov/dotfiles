(local packer (require :packer))
(local use packer.use)

(fn packages[]
  (use :udayvir-singh/tangerine.nvim)
  (use :gbprod/yanky.nvima
       {:config (fn []
                  (local {: setup} (require :floppy.configs.yanky))
                  (setup)
                  )
       }
       ; "nvim-orgmode/orgmode" {:ft :org :config (fn [] (local orgmode (require :orgmode)) )}
       ;   use({
       ;     ,
       ;     ft = { "org" },
       ;     config = function()
       ;         require("orgmode").setup_ts_grammar()
       ;         require("orgmode").setup({})
       ;     end,
       ; })
       ))

(packer.startup)

;                                   use({
;     "nvim-orgmode/orgmode",
;     ft = { "org" },
;     config = function()
;         require("orgmode").setup_ts_grammar()
;         require("orgmode").setup({})
;     end,
; })

; use({
;     "ray-x/lsp_signature.nvim",
;     config = function()
;         local cfg = {
;             bind = true,
;             doc_lines = 10,
;             floating_window = true,
;             floating_window_above_cur_line = true,
;             floating_window_off_x = 0,
;             floating_window_off_y = 0,
;             fix_pos = false,
;             hint_enable = false,
;             max_height = 12,

;             max_width = 80,
;             handler_opts = {
;                 border = "rounded",
;             },

;             always_trigger = false,
;         }

;         require("lsp_signature").setup(cfg)
;     end,
; })

; use({
;     "nmac427/guess-indent.nvim",
;     config = function()
;         require("guess-indent").setup({})
;     end,
; })

; use({
;     "ggandor/leap.nvim",
;     requires = "tpope/vim-repeat",
;     config = function()
;         require("floppy.configs.leap").setup()
;     end,
; })

; use({
;     "klen/nvim-config-local",
;     config = function()
;         require("config-local").setup({
;             config_files = { ".vimrc.lua", ".vimrc" },
;             hashfile = vim.fn.stdpath("data") .. "/config-local",
;             autocommands_create = true,
;             commands_create = true,
;             silent = false,
;         })
;     end,
; })

; use({
;     "https://github.com/TimUntersberger/neogit",
;     config = function()
;         local neogit = require("neogit")
;         neogit.setup({})
;     end,
; })

; use({
;     "untitled-ai/jupyter_ascending.vim",
;     config = function()
;         vim.keymap.set("", "<Leader>e", "<Plug>JupyterExecute")
;     end,
; })

; use("iamcco/markdown-preview.nvim")

; use({
;     "goerz/jupytext.vim",
