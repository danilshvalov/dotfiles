vim.loader.enable()

require("kit.core")

_G.setup = function(name, opts)
  return require(name).setup(opts)
end

-- disable any backup
vim.o.swapfile = false
vim.o.backup = false
vim.o.writebackup = false

vim.o.autoread = true

vim.o.shada = "!,'10000,<50,s10,h"

-- enable undo after closing vim
vim.o.undofile = true

vim.o.smoothscroll = true

vim.o.termguicolors = true

-- enable line to control text width
vim.o.colorcolumn = "80,120"

-- show tabs
vim.o.list = true
vim.opt.listchars = {
  tab = ">-",
}

-- vim.o.ttimeoutlen = 10

vim.o.wrap = false
vim.o.breakindent = true
vim.o.lbr = true
vim.o.history = 10000
-- vim.o.updatetime = 100
vim.o.mouse = "nv"

vim.o.pumheight = 5
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.clipboard = "unnamedplus"

-- enable cursor line highlight
vim.o.cursorline = true

vim.o.cinoptions = vim.o.cinoptions .. "L0"

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup("plugins", {
  colorscheme = { "tokyonight" },
  change_detection = {
    notify = false,
  },
})

_G.map = setmetatable({}, {
  __index = function(_, key)
    return require("keymap").map[key]
  end,
})

vim.o.spell = true
vim.o.spelllang = "en,ru"
vim.o.spellcapcheck = ""
vim.o.spelloptions = "camel"
vim.o.spellsuggest = "best,7"
vim.o.shiftwidth = 4
vim.o.tabstop = 4
vim.o.softtabstop = 4
vim.o.expandtab = true
vim.o.smartindent = true
vim.o.autoindent = true
vim.o.smarttab = true
vim.o.ignorecase = true
vim.o.incsearch = true
vim.o.smartcase = true
vim.o.hlsearch = true
vim.o.scrolloff = 10
vim.o.sidescrolloff = 20
vim.o.signcolumn = "yes"
vim.o.number = true
vim.o.relativenumber = true

kit.autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank({
      higroup = "IncSearch",
      timeout = 150,
    })
  end,
})

local function set_current_line(new_line)
  vim.fn.setline(".", new_line)
  local pos = vim.fn.getpos(".")
  -- the third value is the columns
  pos[3] = vim.fn.col("$")
  vim.fn.setpos(".", pos)
end

local function insert_item()
  local ts_utils = require("nvim-treesitter.ts_utils")

  if vim.bo.filetype ~= "tex" then
    return
  end

  local node = ts_utils.get_node_at_cursor()

  while node do
    local first_line = vim.split(vim.treesitter.get_node_text(node, 0), "\n")[1]
    if node:type() == "generic_environment" then
      local cur_line = vim.fn.getline(".")
      if #cur_line:gsub("^%s*", "", 1) ~= 0 then
        vim.fn.append(vim.fn.line("."), string.rep(" ", vim.fn.indent(".")))
        local pos = vim.api.nvim_win_get_cursor(0)
        vim.api.nvim_win_set_cursor(0, { pos[1] + 1, pos[2] })
        cur_line = vim.fn.getline(vim.fn.line("."))
      end
      if first_line:match("table") then
        set_current_line(cur_line .. "\\hline")
      elseif first_line:match("itemize") or first_line:match("enumerate") then
        set_current_line(cur_line .. "\\item ")
      end
      break
    end
    node = node:parent()
  end
end

kit.create_cmd("Open", function(opts)
  kit.open_file(string.format('"%s"', vim.fn.expand(opts.args)))
end, {
  nargs = "?",
  complete = "file",
})

kit.autocmd("BufWinEnter", {
  pattern = { "*.pdf", "*.png", "*.jpg", "*.heic" },
  callback = function()
    kit.open_file("%")
    MiniBufremove.delete()
  end,
})

kit.open_file = function(path)
  vim.fn.system("open " .. vim.fn.expand(path))
end

vim.o.diffopt = "internal,filler,closeoff,linematch:60,horizontal,foldcolumn:0"

vim.opt.fillchars:append("diff: ")

kit.autocmd("BufWinEnter", {
  desc = "return cursor to where it was last time closing the file",
  pattern = "*",
  command = 'silent! normal! g`"zv',
})

kit.autocmd("TermOpen", {
  callback = function()
    vim.wo.spell = false
  end,
})

function _G.make_comment(str)
  return vim.bo.commentstring:gsub("%%s", str)
end

-- local function uuidgen()
--   return vim.trim(vim.system({ "uuidgen" }, { text = true }):wait().stdout)
-- end

-- local function insert_uuid()
--   local pos = vim.api.nvim_win_get_cursor(0)
--   vim.api.nvim_buf_set_text(0, pos[1] - 1, pos[2] + 1, pos[1] - 1, pos[2] + 1, { uuidgen() })
-- end

map:ft("tex"):mode("i"):set("<A-CR>", insert_item)
--       map:prefix("<leader>i"):set("u", insert_uuid)

vim.g.netrw_keepdir = 0
vim.g.netrw_winsize = 30
vim.g.netrw_altv = 1
vim.g.netrw_banner = 0

vim.g.netrw_list_hide = [[^\.\.\=/\=$]]

kit.call_at_ft({ "markdown", "org" }, function()
  vim.bo.textwidth = 80
end)

vim.cmd.cabbrev("ц w")
vim.cmd.cabbrev("й q")
vim.cmd.cabbrev("цй wq")
