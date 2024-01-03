vim.loader.enable()

table.unpack = unpack

require("kit.core")

-- disable any backup
vim.o.swapfile = false
vim.o.backup = false
vim.o.writebackup = false

vim.o.autoread = true

vim.o.shada = "!,'10000,<50,s10,h"

vim.o.undofile = true

vim.o.smoothscroll = true

vim.o.termguicolors = true

vim.o.colorcolumn = "80"

-- show tabs
vim.o.list = true
vim.opt.listchars = {
  tab = ">-",
}

vim.o.wrap = true
vim.o.breakindent = true
vim.o.lbr = true
vim.o.history = 10000
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
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

local plugins = require("plugins")

for _, plug in ipairs(plugins) do
  local init = plug.init
  plug.init = function(...)
    local args = { ... }

    if plug.keymap then
      -- try to load langmapper first
      pcall(require, "langmapper")
      local ok, keymap = pcall(require, "keymap")
      if ok then
        plug.keymap(keymap.map)
      end
    end

    if init then
      init(unpack(args))
    end
  end
end

require("lazy").setup(plugins, {
  colorscheme = { "tokyonight" },
  change_detection = {
    notify = false,
  },
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

map:ft("tex"):mode("i"):set("<A-CR>", insert_item)

kit.call_at_ft({ "markdown", "org", "tex" }, function()
  vim.bo.textwidth = 80
end)

vim.cmd.cabbrev("ц w")
vim.cmd.cabbrev("й q")
vim.cmd.cabbrev("цй wq")

function _G.open_in_arcadia()
  local path = vim.api.nvim_buf_get_name(0)
end
