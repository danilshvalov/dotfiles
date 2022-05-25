-- :fennel:1652547601
vim.opt["swapfile"] = false
vim.opt["backup"] = false
vim.opt["writebackup"] = false
vim.opt["autoread"] = true
vim.opt["undofile"] = true
vim.opt["ignorecase"] = true
vim.opt["scrolloff"] = 10
vim.opt["sidescrolloff"] = 20
vim.opt["termguicolors"] = true
vim.opt["inccommand"] = ""
vim.opt["tabstop"] = 4
vim.opt["shiftwidth"] = 4
vim.opt["softtabstop"] = 4
vim.opt["expandtab"] = true
vim.opt["smartindent"] = true
vim.opt["autoindent"] = true
vim.opt["smarttab"] = true
vim.opt["number"] = true
vim.opt["relativenumber"] = true
vim.opt["ignorecase"] = true
vim.opt["incsearch"] = true
vim.opt["smartcase"] = true
vim.opt["hlsearch"] = true
vim.opt["lazyredraw"] = true
vim.opt["ttimeoutlen"] = 10
vim.opt["wrap"] = true
vim.opt["breakindent"] = true
vim.opt["formatoptions"] = "l"
vim.opt["lbr"] = true
vim.opt["history"] = 10000
vim.opt["updatetime"] = 100
vim.opt["mouse"] = "nv"
vim.opt["completeopt"] = "menuone,preview,noinsert"
vim.opt["pumheight"] = 5
vim.opt["hidden"] = true
vim.opt["splitbelow"] = true
vim.opt["splitright"] = true
vim.opt["clipboard"] = "unnamedplus"
vim.opt["spell"] = false
vim.opt["spelllang"] = "en,ru"
vim.api.nvim_set_hl(0, "SpellBad", { fg = "#f7768e" })
do end
(vim.opt)["spellcapcheck"] = ""
vim.opt["cursorline"] = true
vim.opt["langmap"] = "\208\164\208\152\208\161\208\146\208\163\208\144\208\159\208\160\208\168\208\158\208\155\208\148\208\172\208\162\208\169\208\151\208\153\208\154\208\171\208\149\208\147\208\156\208\166\208\167\208\157\208\175;ABCDEFGHIJKLMNOPQRSTUVWXYZ,\209\132\208\184\209\129\208\178\209\131\208\176\208\191\209\128\209\136\208\190\208\187\208\180\209\140\209\130\209\137\208\183\208\185\208\186\209\139\208\181\208\179\208\188\209\134\209\135\208\189\209\143;abcdefghijklmnopqrstuvwxyz"
do end
(vim.opt.cinoptions):append("L0")
do end
(vim.opt)["signcolumn"] = "yes"
vim.api.nvim_create_autocmd({ "FileType" }, { command = "wincmd T", pattern = "help" })
vim.api.nvim_create_autocmd({ "BufWritePost" }, { command = "silent exec \"!sass %:p %:r.css\"", pattern = "[^_]*.sass,[^_]*.scss" })
return vim.api.nvim_create_autocmd({ "SourceCmd" }, { command = ":FnlFile <afile>:p", pattern = "*.fnl" })
