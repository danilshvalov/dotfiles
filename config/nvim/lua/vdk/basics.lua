local hi = require("vdk.core.utils").hi

vim.o.swapfile = false
vim.o.backup = false
vim.o.writebackup = false
vim.o.autoread = true
vim.o.undofile = true
vim.o.ignorecase = true

vim.api.nvim_command([[
" Trigger `autoread` when files changes on disk
autocmd FocusGained,BufEnter,CursorHold,CursorHoldI * if mode() != 'c' | checktime | endif
" Notification after file change
autocmd FileChangedShellPost *
  \ echohl WarningMsg | echo "File changed on disk. Buffer reloaded." | echohl None
]])

vim.o.scrolloff = 10
vim.o.sidescrolloff = 20

vim.o.termguicolors = true
vim.o.inccommand = ""

vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.softtabstop = 4
vim.o.expandtab = true
vim.o.smartindent = true
vim.o.autoindent = true
vim.o.smarttab = true
vim.o.number = true
vim.o.relativenumber = true

vim.o.ignorecase = true
vim.o.incsearch = true
vim.o.smartcase = true
vim.o.hlsearch = true

vim.o.lazyredraw = true
vim.o.ttimeoutlen = 10

vim.o.wrap = true
vim.o.breakindent = true
vim.o.formatoptions = "l"
vim.o.lbr = true
vim.o.history = 10000
vim.o.updatetime = 100
vim.o.mouse = "nv"
vim.o.completeopt = "menuone,preview,noinsert"

vim.o.pumheight = 5
vim.o.hidden = true
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.clipboard = "unnamedplus"
vim.o.spell = false
vim.o.spelllang = "en,ru"

hi("SpellBad", { fg = "#f7768e", underline = true })

vim.o.spellcapcheck = ""

vim.o.cursorline = true

vim.olangmap =
    "ФИСВСВСВСВСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz"

vim.o.cinoptions = vim.o.cinoptions .. "L0"

vim.o.signcolumn = "yes"

vim.cmd("autocmd FileType help wincmd T")

-- ; (vim.cmd "autocmd BufWritePost [^_]*.sass,[^_]*.scss silent exec \"!sass %"p" %"r".css\"")
-- (autocmd! [BufWritePost] "[^_]*.sass,[^_]*.scss"
--           "silent exec \"!sass %"p" %"r".css\"")
