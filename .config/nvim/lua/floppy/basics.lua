-- ################# Basic settings ################ --
-- ================= File management ================= --
-- swapfile has global & local config, eventhough help says otherwise
vim.o.swapfile = false -- can open already open files
vim.bo.swapfile = false
vim.o.backup = false
vim.o.writebackup = false
vim.o.autoread = true -- auto file change detection
vim.o.undofile = true
vim.o.ignorecase = true

-- autocmds are currently not supported by nvim (0.5 nighlty)
vim.api.nvim_command([[
" Triger `autoread` when files changes on disk
autocmd FocusGained,BufEnter,CursorHold,CursorHoldI * if mode() != 'c' | checktime | endif
" Notification after file change
autocmd FileChangedShellPost *
  \ echohl WarningMsg | echo "File changed on disk. Buffer reloaded." | echohl None
]])

-- vim.api.nvim_command("filetype plugin indent on")

-- ================= Scrolling ================= --

vim.o.scrolloff = 10 -- start scrolling when 10 lines away from margins
vim.o.sidescrolloff = 20

-- ================= Visualization ================= --

vim.o.termguicolors = true
vim.o.inccommand = ""

-- ================= Indentation ================= --

-- pay attention to 'vim.bo' (buffer local options) and 'vim.o' (global options)
-- see :help options.txt

-- for some reason these values need to be set in both o and bo objects
-- eventhough these options are supposed to be local to buffer
vim.o.tabstop = 4 -- maximum width of tab character (measured in spaces)
-- vim.bo.tabstop = 4
vim.o.shiftwidth = 4 -- size of indent (measured in spaces), should equal tabstop
-- vim.bo.shiftwidth = 4
vim.o.softtabstop = 4 -- should be the same as the other two above
-- vim.bo.softtabstop = 4
vim.o.expandtab = true -- expand tabs to spaces
-- vim.bo.expandtab = true -- expand tabs to spaces
vim.o.smartindent = true -- smart indenting on new line for C-like programs
-- vim.bo.smartindent = true
vim.o.autoindent = true -- copy the indentation from previous line
-- vim.bo.autoindent = true
vim.o.smarttab = true -- tab infront of a line inserts blanks based on shiftwidth

-- ================= Number column ================= --

vim.wo.number = true
vim.o.relativenumber = true

-- ================= Search ================= --

vim.o.ignorecase = true -- Ignorecase when searching
vim.o.incsearch = true -- start searching on each keystroke
vim.o.smartcase = true -- ignore case when lowercase, match case when capital case is used
vim.o.hlsearch = true -- highlight the search results

-- ================= Performance ================= --

vim.o.lazyredraw = true -- useful for when executing macros.
vim.o.ttimeoutlen = 10 -- ms to wait for a key code seq to complete

-- ================= Misc ================= --

vim.wo.wrap = true -- wrap long text into multiple lines
vim.o.breakindent = true
vim.o.formatoptions = "l"
vim.o.lbr = true
vim.o.history = 10000 -- numbers of entries in history for ':' commands and search patterns (10000 = max)
vim.o.updatetime = 100 -- used for CursorHold event (for document highlighting detection)
vim.o.mouse = "nv" -- allow mose in normal & visual mode

-- better autocomplete behaviour
-- menuone - show popup menu also when there is only one match available
-- preview - show extra information about currently selected completion
-- noinsert - do not insert any text for match until the user selects it from the menu
vim.o.completeopt = "menuone,preview,noinsert"

-- preview menu size
vim.o.pumheight = 5

-- allows hidden buffers
-- this means that a modified buffer doesn't need to be saved when changing
-- tabs/windows.
vim.o.hidden = true

-- horizontal split: open new buffer under current buffer
vim.o.splitbelow = true
-- vertical split: open new buffer right current buffer
vim.o.splitright = true

-- Copy paste between vim and everything else
vim.o.clipboard = "unnamedplus"

-- Indent view
vim.o.list = true
vim.o.listchars = "space:·"

-- vim.api.nvim_command("highlight ColorColumn guibg=#8b0000")

-- spelling
vim.o.spell = false
vim.o.spelllang = "en,ru"
vim.api.nvim_command("highlight SpellBad gui=underline guifg=#f7768e")
vim.o.spellcapcheck = ""

-- cursor highlighting
vim.o.cursorline = true
--

vim.o.langmap =
    "ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz"

-- vim.api.nvim_command("autocmd BufNewFile,BufRead *.log :set filetype=log")

-- vim.api.nvim_exec(
--     [[
-- augroup highlight_yank
--     autocmd!
--     au TextYankPost * silent! lua vim.highlight.on_yank({higroup="IncSearch", on_visual = false})
-- augroup END
-- ]],
--     false
-- )

-- vim.o.statusline = "%1* %F "
--     .. "%M"
--     -- .. "%2*"
--     .. "%1*%=%{''.(&fenc!=''?&fenc:&enc).''} "
--     .. "%{&ff}      "
--     .. "%4l:%c "
--     .. "%3p%% "

-- vim.o.whichwrap = vim.o.whichwrap .. "<,>,h,l"

-- disable removing indent if you insert colon
vim.o.cinoptions = vim.o.cinoptions .. "L0"

-- enable padding for icons
vim.o.signcolumn = "yes"

vim.cmd("autocmd FileType help wincmd T")

vim.cmd('autocmd BufWritePost [^_]*.sass,[^_]*.scss  silent exec "!sass --style compressed %:p %:r.css"')
