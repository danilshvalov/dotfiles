(import-macros {: set! : set+ : hi! : autocmd!} :dna.vim)

(set! noswapfile)
(set! nobackup)
(set! nowritebackup)
(set! autoread)
(set! undofile)
(set! ignorecase)

; vim.api.nvim_command([[
; " Triger `autoread` when files changes on disk
; autocmd FocusGained,BufEnter,CursorHold,CursorHoldI * if mode() != 'c' | checktime | endif
; " Notification after file change
; autocmd FileChangedShellPost *
;   \ echohl WarningMsg | echo "File changed on disk. Buffer reloaded." | echohl None
; ]])

(set! scrolloff 10)
(set! sidescrolloff 20)

(set! termguicolors)
(set! inccommand "")

(set! tabstop 4)
(set! shiftwidth 4)
(set! softtabstop 4)
(set! expandtab)
(set! smartindent)
(set! autoindent)
(set! smarttab)
(set! number)
(set! relativenumber)

(set! ignorecase)
(set! incsearch)
(set! smartcase)
(set! hlsearch)

(set! lazyredraw)
(set! ttimeoutlen 10)

(set! wrap)
(set! breakindent)
(set! formatoptions :l)
(set! lbr)
(set! history 10000)
(set! updatetime 100)
(set! mouse :nv)
(set! completeopt "menuone,preview,noinsert")

(set! pumheight 5)
(set! hidden)
(set! splitbelow)
(set! splitright)
(set! clipboard :unnamedplus)
; (set! list)
; (set! listchars "space:·")
(set! nospell)
(set! spelllang "en,ru")

(hi! SpellBad {:fg "#f7768e"} [underline])

(set! spellcapcheck "")

(set! cursorline)

(set! langmap
      "ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz")

(set+ cinoptions :L0)

(set! signcolumn :yes)

(autocmd! [FileType] :help "wincmd T")

; (vim.cmd "autocmd BufWritePost [^_]*.sass,[^_]*.scss silent exec \"!sass %:p %:r.css\"")
(autocmd! [BufWritePost] "[^_]*.sass,[^_]*.scss"
          "silent exec \"!sass %:p %:r.css\"")

(autocmd! [SourceCmd] :*.fnl ":FnlFile <afile>:p")

; (autocmd! [BufWritePost] :*.tex "silent exec \"!make\"")
